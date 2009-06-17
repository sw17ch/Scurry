#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#include <net/route.h>
#include <netinet/if_ether.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>

#include "help.h"

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti);
int get_mac(struct ifreq * ifr, int sock, struct tap_info * ti);
void close_tap(union tap_desc * td);

static int set_ip(struct ifreq * ifr, int sock, ip4_addr_t ip4);
static int set_mask(struct ifreq * ifr, int sock, ip4_addr_t ip4);
static int set_mtu(struct ifreq * ifr, int sock, unsigned int mtu);


int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti)
{
    struct ifreq ifr_tap;
    int r = 0;

    int fd = -1;
    int sock = -1;

    if ((fd = open("/dev/net/tun", O_RDWR)) < 0)
        return -1;

    memset(&ifr_tap, 0, sizeof(ifr_tap));

    /* setup tap */
    ifr_tap.ifr_flags = IFF_TAP | IFF_NO_PI;

    if ((ioctl(fd, TUNSETIFF, (void *)&ifr_tap)) < 0)
        return -2;
    
    /* setup ip */
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
        return -3;

    if (set_ip(&ifr_tap, sock, local_ip) < 0)
        return -4;

    if (set_mask(&ifr_tap, sock, local_mask) < 0)
        return -5;

    if ( ioctl(sock, SIOCGIFFLAGS, &ifr_tap) < 0)
        return -6;

    if ( get_mac(&ifr_tap,sock,ti) < 0)
        return -7;

    ifr_tap.ifr_flags |= IFF_UP;
    ifr_tap.ifr_flags |= IFF_RUNNING;

    if (ioctl(sock, SIOCSIFFLAGS, &ifr_tap) < 0)
        return -8;

    if (set_mtu(&ifr_tap, sock, 1200) < 0)
        return -9;

    ti->desc->desc = fd;

    return fd;
}

static int set_ip(struct ifreq * ifr, int sock, ip4_addr_t ip4)
{
    struct sockaddr_in addr;

    /* set the IP of this end point of tunnel */
    memset( &addr, 0, sizeof(addr) );
    addr.sin_addr.s_addr = ip4; /*network byte order*/
    addr.sin_family = AF_INET;
    memcpy( &ifr->ifr_addr, &addr, sizeof(struct sockaddr) );

    if ( ioctl(sock, SIOCSIFADDR, ifr) < 0) {
        printf("SIOCSIFADDR: %s\n", strerror(errno));
        return -1;
    }

    return 0; 
}

static int set_mask(struct ifreq * ifr, int sock, ip4_addr_t ip4)
{
    struct sockaddr_in addr;

    memset( &addr, 0, sizeof(addr) );
    addr.sin_addr.s_addr = ip4; /*network byte order*/
    addr.sin_family = AF_INET;
    memcpy( &ifr->ifr_addr, &addr, sizeof(struct sockaddr) );
    
    if ( ioctl(sock, SIOCSIFNETMASK, ifr) < 0) {
        printf("SIOCSIFNETMASK: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

static int set_mtu(struct ifreq * ifr, int sock, unsigned int mtu)
{
    /* Set the MTU of the tap interface */
    ifr->ifr_mtu = mtu; 
    if (ioctl(sock, SIOCSIFMTU, ifr) < 0)  {
        printf("SIOCSIFMTU: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

void close_tap(union tap_desc * td)
{
    if (0 <= td->desc) 
    {
        close(td->desc);
    }
}

int get_mac(struct ifreq * ifr, int sock, struct tap_info * ti)
{
    
    if ( ioctl(sock, SIOCGIFHWADDR, ifr) < 0) {
        printf("SIOCGIFHWADDR: %s\n", strerror(errno));
        return -1;
    }
    else
    {
        memcpy(&(ti->mac),&(ifr->ifr_hwaddr.sa_data),6);
    }


    return 0;
}

/* I HATE WINDOWS IT SUCKS SO HARD AHHH!! */
int read_tap(union tap_desc * td, char * buf, int len)
{
    int ret = read(td->desc,buf,len);
    return ret;
}

int write_tap(union tap_desc * td, const char * buf, int len)
{
    int ret = write(td->desc,buf,len);
    return ret;
}
