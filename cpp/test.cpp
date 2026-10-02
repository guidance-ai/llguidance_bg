#include "llguidance_bg_cpp.h"

#include <iostream>
#include <cassert>

int main() {
  try {
    llguidance_bg::ConstraintMgrConfig cfg(12000);
    cfg.eos_token_name = "<|eos|>";
    cfg.get_token_bytes = [](uint32_t token_id) {
      // this makes no sense...
      std::vector<uint8_t> token_bytes;
      token_bytes.push_back(token_id);
      return token_bytes;
    };
    llguidance_bg::ConstraintMgr mgr;
    mgr.init(cfg);
    std::string grammar =
        "{ \"grammars\": [{ \"lark_grammar\": \"start: /.*/\" }] }";
    auto constraint = mgr.create_constraint(grammar.c_str(), grammar.size());
    auto c2 = bllg_clone_constraint(constraint);
    auto c3 = bllg_clone_constraint(constraint);
    auto c4 = bllg_clone_constraint(constraint);
    auto cancellation2 = bllg_get_cancellation_handle(c2);
    auto cancellation3 = bllg_get_cancellation_handle(c3);
    auto cancellation4 = bllg_get_cancellation_handle(c4);
    assert(cancellation2 != nullptr);
    assert(cancellation3 != nullptr);
    assert(cancellation4 != nullptr);
    bllg_free_constraint(constraint);
    bllg_cancel(cancellation2);
    const uint32_t *ff_tokens = nullptr;
    assert(bllg_compute_ff_tokens(c2, &ff_tokens) == -1);
    bllg_cancel(cancellation3);
    assert(bllg_start_compute_mask(c3, nullptr, nullptr) == -1);
    bllg_free_constraint(c2);
    bllg_free_constraint(c3);
    bllg_free_constraint(c4);
    assert(bllg_is_cancelled(cancellation2));
    assert(bllg_is_cancelled(cancellation3));
    bllg_cancel(cancellation4);
    assert(bllg_is_cancelled(cancellation4));
    bllg_free_cancellation_handle(cancellation2);
    bllg_free_cancellation_handle(cancellation3);
    bllg_free_cancellation_handle(cancellation4);

    std::string g2 = "{ foobar";
    std::string msg;
    auto valid = mgr.validate_grammar(g2.c_str(), g2.size(), msg);
    assert(!valid);
    assert(msg.find("key must be a string") != std::string::npos);

    valid = mgr.validate_grammar(grammar.c_str(), grammar.size(), msg);
    assert(valid);
    assert(msg.empty());

    std::string j1 = R"(
      start: %json {
        "x-guidance": {
          "lenient": true
        },
        "oneOf": [
          { "type": "object", "properties": { "foo": { "type": "string" } }, "additionalProperties": true },
          { "type": "object", "properties": { "bar": { "type": "string" } }, "additionalProperties": true }
        ]
      }
    )";

    valid = mgr.validate_grammar(j1.c_str(), j1.size(), msg);
    assert(valid);
    assert(msg.starts_with("WARNING:"));

  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}