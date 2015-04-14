#include "vxi11_lib/vxi11_user.h"


/* callable from C */

extern "C" {
    int open_device(const char *ip, CLINK *clink)
    {
	vxi11_open_device(ip, clink);
    }

    int	open_device_by_name(const char *ip, CLINK *clink, char *device)
    {
	vxi11_open_device(ip,clink,device);
    }
    int	close_device(const char *ip, CLINK *clink)
    {
	vxi11_close_device(ip, clink);
    }

    int	send_command(CLINK *clink, const char *cmd)
    {
	vxi11_send(clink,cmd);
    }
    
    int send_data(CLINK *clink, const char *cmd, unsigned long len)
    {
	vxi11_send(clink,cmd,len);
    }

    long receive(CLINK *clink, char *buffer, unsigned long len, unsigned long timeout)
    {
	vxi11_receive(clink,buffer,len,timeout);
    }

    int	send_data_block(CLINK *clink, const char *cmd, char *buffer, unsigned long len)
    {
	vxi11_send_data_block(clink,cmd,buffer,len);
    }

    long receive_data_block(CLINK *clink, char *buffer, unsigned long len, unsigned long timeout)
    {
	vxi11_receive_data_block(clink,buffer,len,timeout);
    }

    long send_and_receive(CLINK *clink, const char *cmd, char *buf, unsigned long buf_len, unsigned long timeout)
    {
	vxi11_send_and_receive(clink,cmd,buf,buf_len,timeout);
    }

    long obtain_long_value(CLINK *clink, const char *cmd, unsigned long timeout)
    {
	vxi11_obtain_long_value(clink,cmd,timeout);
    }

    double obtain_double_value(CLINK *clink, const char *cmd, unsigned long timeout)
    {
	vxi11_obtain_double_value(clink,cmd,timeout);
    }

}


