#ifndef __HELP_HASKELL_VPN__
#define __HELP_HASKELL_VPN__ __HELP_HASKELL_VPN__

#include <assert.h>

typedef uint32_t ip4_addr_t;

#define TAP_DESC_SIZE 32

/* A union that serves to be a common storage medium for
 * whatever the OS wants to use as a device descriptor
 * for the TAP device. In Linux, this is just a file 
 * descriptor. */
union tap_desc {
    char pad[TAP_DESC_SIZE];
#if defined(MINGW32)
    struct
    {
      HANDLE desc;
      unsigned long context;
    };
#elif defined(LINUX)
    int desc;
#elif defined(BSD)
    int desc;
#else
    #error "ERROR: This platform not supported."
#endif
};

/* A struct used to pass information about
 * a tap device around. */
struct tap_info {
    union tap_desc * desc;
    char mac[8]; /* Use 8, instead of 6, to force alignment */
};

/* Open a tap device. The file descriptor and the
 * MAC address are written to the tap_info struct.
 * On failure, a negative value is returned. */
int open_tap(ip4_addr_t local_ip,
             ip4_addr_t local_mask,
             struct tap_info * ti);

/* Close a tap device. */
void close_tap(union tap_desc * td);

/* Read a frame from a tap device. The buffer needs
 * to be at least as large as the MTU of the device. */
int read_tap(union tap_desc * td, char * buf, int len);

/* Write a frame to a tap device. The frame length
 * must be less than the MTU of the device. */
int write_tap(union tap_desc * td, const char * buf, int len);

#endif /* __HELP_HASKELL_VPN__ */

