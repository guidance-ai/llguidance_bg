#include "llguidance_bg_cpp.h"

#include <iostream>

int main() {
  try {
    llguidance_bg::ConstraintMgrConfig cfg(128000);
    cfg.eos_token_name = "<|eos|>";
    cfg.get_token_bytes = [](uint32_t token_id) {
      // this makes no sense...
      std::vector<uint8_t> token_bytes;
      token_bytes.push_back(token_id);
      return token_bytes;
    };
    llguidance_bg::ConstraintMgr mgr;
    mgr.init(cfg);
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}