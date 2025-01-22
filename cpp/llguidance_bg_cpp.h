#ifndef LLGUIDANCE_BG_CPP_H
#define LLGUIDANCE_BG_CPP_H

#include <string>
#include <vector>
#include <stdexcept>
#include <memory>
#include <unordered_map>
#include <functional>

#include "llguidance.h"
#include "llguidance_bg.h"

namespace llguidance_bg {

struct ConstraintMgrConfig {
  uint32_t n_vocab;
  std::string eos_token_name;
  std::function<std::vector<uint8_t>(uint32_t)> get_token_bytes;

  LlgTokenizeFn tokenize_fn;
  void *tokenize_user_data;
  bool tokenize_assumes_string;

  size_t num_threads;

  LlgConstraintInit cinit;
  std::vector<std::string> slices;

  ConstraintMgrConfig(uint32_t n_vocab)
      : n_vocab(n_vocab), tokenize_fn(nullptr), tokenize_user_data(nullptr),
        num_threads(0), tokenize_assumes_string(true) {
    llg_constraint_init_set_defaults(&cinit, nullptr);
  }
};

class ConstraintMgr {
public:
  ConstraintMgr() {
    tokenizer = nullptr;
    constraint_mgr = nullptr;
  }

  void init(const ConstraintMgrConfig &cfg) {
    if (cfg.n_vocab == 0) {
      throw std::invalid_argument("n_vocab must be greater than 0");
    }

    if (cfg.eos_token_name.size() == 0) {
      throw std::invalid_argument("eos_token_name must be set");
    }

    if (!cfg.get_token_bytes) {
      throw std::invalid_argument("get_token_bytes must be set");
    }

    char error_string[1024];

    auto n_vocab = cfg.n_vocab;
    LlgTokenizerInit init = {};
    init.vocab_size = n_vocab;
    init.tokenize_assumes_string = cfg.tokenize_assumes_string;

    if (cfg.tokenize_fn) {
      init.tokenize_fn = cfg.tokenize_fn;
      init.tokenize_user_data = cfg.tokenize_user_data;
    } else {
      init.use_approximate_greedy_tokenize_fn = true;
    }

    // serialize tokens in format required by llguidance
    std::vector<uint8_t> token_bytes;
    std::vector<uint32_t> token_lens;
    for (uint32_t token_id = 0; token_id < n_vocab; token_id++) {
      auto token = cfg.get_token_bytes(token_id);
      if (token.size() == 0) {
        std::string unresolved_token =
            "<|tok_" + std::to_string(token_id) + "|>";
        token_lens.push_back(1 + unresolved_token.size());
        token_bytes.push_back(0xFF);
        token_bytes.insert(token_bytes.end(), unresolved_token.begin(),
                           unresolved_token.end());
      } else {
        // check if the token starts with "<|" and ends with "|>"
        if (token.size() >= 5 && token[0] == '<' && token[1] == '|' &&
            token[token.size() - 2] == '|' && token[token.size() - 1] == '>') {
          // special tokens are marked with 0xFF byte
          token_lens.push_back(token.size() + 1);
          token_bytes.push_back(0xFF);
          if (token.size() == cfg.eos_token_name.size() &&
              memcmp(token.data(), cfg.eos_token_name.data(),
                     cfg.eos_token_name.size()) == 0) {
            init.tok_eos = token_id;
          }
        } else {
          token_lens.push_back(token.size());
        }
        token_bytes.insert(token_bytes.end(), token.begin(), token.end());
      }
    }
    init.token_bytes = token_bytes.data();
    init.token_lens = token_lens.data();

    tokenizer = llg_new_tokenizer(&init, error_string, sizeof(error_string));
    if (tokenizer == nullptr) {
      throw std::invalid_argument("Error creating tokenizer: " +
                                  std::string(error_string));
    }

    LlgConstraintInit cinit = cfg.cinit;
    cinit.tokenizer = tokenizer;

    std::vector<const char *> slices;
    for (const auto &slice : cfg.slices) {
      slices.push_back(slice.c_str());
    }
    slices.push_back(nullptr);

    constraint_mgr =
        bllg_new_constraint_mgr(&cinit, cfg.num_threads, slices.data(),
                                error_string, sizeof(error_string));
    if (constraint_mgr == nullptr) {
      throw std::invalid_argument("Error creating constraint manager: " +
                                  std::string(error_string));
    }
  }

  ~ConstraintMgr() {
    if (constraint_mgr != nullptr) {
      bllg_free_constraint_mgr(constraint_mgr);
      constraint_mgr = nullptr;
    }
    if (tokenizer != nullptr) {
      llg_free_tokenizer(tokenizer);
      tokenizer = nullptr;
    }
  }

  BllgConstraint *create_constraint(const char *grammar, size_t grammar_size) {
    BllgConstraint *constraint = bllg_new_constraint(
        constraint_mgr, (const uint8_t *)grammar, grammar_size);
    auto error_ptr = bllg_get_error(constraint);
    if (error_ptr != nullptr) {
      auto error = std::string(error_ptr);
      bllg_free_constraint(constraint);
      // TODO: is this how we should handle errors?
      throw std::invalid_argument("Error creating constraint: " + error);
    }
    return constraint;
  }

  // utility methods
  std::string stringify_tokens(const std::vector<uint32_t> &tokens) const {
    char output[1024];
    size_t len = llg_stringify_tokens(tokenizer, tokens.data(), tokens.size(),
                                      output, sizeof(output));
    if (len >= sizeof(output)) {
      len = sizeof(output) - 1;
    }
    return std::string(output, len);
  }

  std::vector<uint32_t> tokenize_bytes(const uint8_t *byte_ptr,
                                       size_t byte_len) const {
    std::vector<uint32_t> tokens(byte_len / 4 + 20);
    size_t n_tokens = llg_tokenize_bytes(tokenizer, byte_ptr, byte_len,
                                         tokens.data(), tokens.size());
    if (n_tokens > tokens.size()) {
      tokens.resize(n_tokens);
      n_tokens = llg_tokenize_bytes(tokenizer, byte_ptr, byte_len,
                                    tokens.data(), tokens.size());
    } else {
      tokens.resize(n_tokens);
    }
    return tokens;
  }

  std::vector<uint32_t> tokenize_bytes_marker(const uint8_t *byte_ptr,
                                              size_t byte_len) const {
    std::vector<uint32_t> tokens(byte_len / 4 + 20);
    size_t n_tokens = llg_tokenize_bytes_marker(tokenizer, byte_ptr, byte_len,
                                                tokens.data(), tokens.size());
    if (n_tokens > tokens.size()) {
      tokens.resize(n_tokens);
      n_tokens = llg_tokenize_bytes_marker(tokenizer, byte_ptr, byte_len,
                                           tokens.data(), tokens.size());
    } else {
      tokens.resize(n_tokens);
    }
    return tokens;
  }

  std::vector<uint32_t> tokenize_string(const std::string &str) const {
    return tokenize_bytes((const uint8_t *)str.data(), str.size());
  }

protected:
  LlgTokenizer *tokenizer;
  BllgConstraintMgr *constraint_mgr;
  friend class StopController;
};

class StopController {
public:
  StopController(ConstraintMgr &mgr, std::vector<uint32_t> stop_tokens,
                 std::optional<std::string> stop_rx = std::nullopt)
      : stop_requested(false) {
    char error_string[1024];
    stop_controller = llg_new_stop_controller(
        mgr.tokenizer, stop_tokens.data(), stop_tokens.size(),
        stop_rx ? stop_rx->c_str() : nullptr, error_string,
        sizeof(error_string));
    if (stop_controller == nullptr) {
      throw std::invalid_argument("Error creating stop controller: " +
                                  std::string(error_string));
    }
  }

  bool is_stopped() const { return stop_requested; }

  std::string commit_token(uint32_t token) {
    size_t output_len = 0;
    const char *r = llg_stop_commit_token(stop_controller, token, &output_len,
                                          &stop_requested);
    if (r == nullptr) {
      return "";
    }
    return std::string(r, output_len);
  }

  ~StopController() {
    if (stop_controller != nullptr) {
      llg_free_stop_controller(stop_controller);
      stop_controller = nullptr;
    }
  }

protected:
  bool stop_requested;
  LlgStopController *stop_controller;
};

} // namespace llguidance_bg

#endif // LLGUIDANCE_BG_CPP_H