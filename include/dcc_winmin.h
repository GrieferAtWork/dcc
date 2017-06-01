/* Copyright (c) 2017 Griefer@Work                                            *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 */
#ifndef GUARD_WINMIN_H
#define GUARD_WINMIN_H 1

/* Don't need any of this $h1t */

#define NOGDICAPMASKS
#define NOVIRTUALKEYCODES
#define NOWINMESSAGES
#define NOWINSTYLES
#define NOSYSMETRICS
#define NOMENUS
#define NOICONS
#define NOKEYSTATES
#define NOSYSCOMMANDS
#define NORASTEROPS
#define NOSHOWWINDOW
#define OEMRESOURCE
#define NOATOM
#define NOCLIPBOARD
#define NOCOLOR
#define NOCTLMGR
#define NODRAWTEXT
#define NOGDI
#define NOKERNEL
#define NOUSER
#define NONLS
#define NOMB
#define NOMEMMGR
#define NOMETAFILE
#define NOMINMAX
#define NOMSG
#define NOOPENFILE
#define NOSCROLL
#define NOSERVICE
#define NOSOUND
#define NOTEXTMETRIC
#define NOWH
#define NOWINOFFSETS
#define NOCOMM
#define NOKANJI
#define NOHELP
#define NOPROFILER
#define NODEFERWINDOWPOS
#define NOMCX

#define NOAPISET
#define NOCRYPT
#define NOIME

#define WIN32_LEAN_AND_MEAN

#ifndef _MSC_VER
/* Disable more C7A9 */
#define _OLE2_H_
#define VER_H
#define _WINREG_
#define _WINNETWK_
#define __STRALIGN_H_
#define _APIAPPCONTAINER_
#define _APISETREALTIME_
#define _PROCESSTOPOLOGYAPI_H_
#define _SYSTEMTOPOLOGY_H_
#define _APISETNAMESPACE_
#define _APISECUREBASE_
//#define _APISETLIBLOADER_ // Stuff crashes without this
#define _WOW64APISET_H_
#define _JOBAPISET_H_
#define _BEM_H_
#define _THREADPOOLAPISET_H_
#define _THREADPOOLLEGACYAPISET_H_
//#define _MEMORYAPI_H_ // We actually need this one
//#define _SYSINFOAPI_H_ // Stuff crashes without this
//#define _PROCESSTHREADSAPI_H_ // Stuff crashes without this
#define _INTERLOCKAPI_H_
#define _SYNCHAPI_H_
#define _IO_APISET_H_
#define _HEAPAPI_H_
#define _PROFILEAPI_H_
#define _NAMEDPIPE_H_
#define _FIBERS_H_
#define _ERRHANDLING_H_
//#define _APISETHANDLE_ // Need this one for CloseHandle()
#define _APISETUTIL_
//#define _APISETDEBUG_ // OutputDebugStringA()
//#define _APISETFILE_ // A buck of file functions
//#define _PROCESSENV_ // GetStdHandle()
#endif

#include <Windows.h>

#undef NOGDICAPMASKS
#undef NOVIRTUALKEYCODES
#undef NOWINMESSAGES
#undef NOWINSTYLES
#undef NOSYSMETRICS
#undef NOMENUS
#undef NOICONS
#undef NOKEYSTATES
#undef NOSYSCOMMANDS
#undef NORASTEROPS
#undef NOSHOWWINDOW
#undef OEMRESOURCE
#undef NOATOM
#undef NOCLIPBOARD
#undef NOCOLOR
#undef NOCTLMGR
#undef NODRAWTEXT
#undef NOGDI
#undef NOKERNEL
#undef NOUSER
#undef NONLS
#undef NOMB
#undef NOMEMMGR
#undef NOMETAFILE
#undef NOMINMAX
#undef NOMSG
#undef NOOPENFILE
#undef NOSCROLL
#undef NOSERVICE
#undef NOSOUND
#undef NOTEXTMETRIC
#undef NOWH
#undef NOWINOFFSETS
#undef NOCOMM
#undef NOKANJI
#undef NOHELP
#undef NOPROFILER
#undef NODEFERWINDOWPOS
#undef NOMCX

#endif /* !GUARD_WINMIN_H */
