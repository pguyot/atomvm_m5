/* SPDX-License-Identifier: MIT */
#ifndef __ATOMVM_M5_H
#define __ATOMVM_M5_H

#include <globalcontext.h>
#include <nifs.h>

void atomvm_m5_init(GlobalContext *global);
const struct Nif *atomvm_m5_get_nif(const char *nifname);

#endif
