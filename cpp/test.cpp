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
    bllg_free_constraint(constraint);
    bllg_free_constraint(c2);

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