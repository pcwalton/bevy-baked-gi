#!/bin/bash
bindgen \
    -o src/ibllib_bindings.rs \
    --allowlist-function IBLSample \
    ibllib-wrapper.h \
    -- \
    -I../../Contrib/glTF-IBL-Sampler/lib/include \
    -I../../Contrib/glTF-IBL-Sampler/thirdparty/Vulkan-Headers/include \
    -x c++
