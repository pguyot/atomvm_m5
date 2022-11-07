# SPDX-License-Identifier: MIT

set(ATOMVM_M5_COMPONENT_SRCS
    "nifs/atomvm_m5.cc"
)

idf_component_register(
    SRCS ${ATOMVM_M5_COMPONENT_SRCS}
    INCLUDE_DIRS "nifs/include"
    PRIV_REQUIRES "libatomvm" "avm_sys" "main" "M5Unified"
)

idf_build_set_property(
    LINK_OPTIONS "-Wl,--whole-archive ${CMAKE_CURRENT_BINARY_DIR}/lib${COMPONENT_NAME}.a -Wl,--no-whole-archive"
    APPEND
)
