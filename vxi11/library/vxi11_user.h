/* vxi11_user.h
 * Copyright (C) 2006 Steve D. Sharples
 *
 * User library for opening, closing, sending to and receiving from
 * a device enabled with the VXI11 RPC ethernet protocol. Uses the files
 * generated by rpcgen vxi11.x.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * The author's email address is steve.sharples@nottingham.ac.uk
 */

#ifndef	_VXI11_USER_H_
#define	_VXI11_USER_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#  ifdef vxi11_EXPORTS
#    define vx_EXPORT __declspec(dllexport)
#  else
#    define vx_EXPORT __declspec(dllimport)
#  endif
#else
#  define vx_EXPORT
#  define __stdcall
#endif

#include <stdlib.h>


#define LIBVXI11_MAJOR 2
#define LIBVXI11_MINOR 0
#define LIBVXI11_REVISION 0
/* LIBVXI11_VERSION_NUMBER looks like 2002001 for e.g. version 2.2.1. */
#define LIBVXI11_VERSION_NUMBER (LIBVXI11_MAJOR*1000000+LIBVXI11_MINOR*1000+LIBVXI11_REVISION)


typedef	struct _VXI11_CLINK VXI11_CLINK;

/* Default timeout value to use, in ms. */
#define	VXI11_DEFAULT_TIMEOUT	10000

/* Read timeout to use, in ms. */
#define	VXI11_READ_TIMEOUT	2000

/* vxi11_receive() return value if a query times out ON THE INSTRUMENT (and so
 * we have to resend the query). */
#define	VXI11_NULL_READ_RESP	50

/* vxi11_send() return value if a sent command times out ON THE INSTRUMENT. */
#define	VXI11_NULL_WRITE_RESP	51


/* Function: vxi11_library_version
 *
 * Return the version of the current library.
 *
 * Parameters:
 *  major -    an integer pointer. If not NULL, the major version of the
 *             library will be returned in this variable.
 *  minor -    an integer pointer. If not NULL, the minor version of the
 *             library will be returned in this variable.
 *  revision - an integer pointer. If not NULL, the revision of the library will
 *             be returned in this variable.
 * 
 * Returns:
 *  LIBVXI11_VERSION_NUMBER, which is a unique number based on the major,
 *      minor and revision values.
 */
vx_EXPORT int vxi11_lib_version(int *major, int *minor, int *revision);


/* Function: vxi11_open_device
 *
 * Open a connection to an instrument.
 *
 * Parameters:
 *  clink   - pointer to a VXI11_CLINK pointer, will be initialised on a
 *            successful connection.
 *  address - the IP address or (where supported) USB address for the
 *            instrument to connect to.
 *  device   - some instruments have multiple interfaces, this allows you to
 *            specify which to connect to. Set to NULL to use the default of
 *            "inst0".
 *
 * Returns:
 *  0 - on success
 *  1 - on failure. clink will not be a valid pointer.
 */
vx_EXPORT int vxi11_open_device(VXI11_CLINK **clink, const char *address, char *device);


/* Function: vxi11_close_device
 *
 * Parameters:
 *  clink   - a valid VXI11_CLINK pointer.
 *  address - the IP address or (where supported) USB address for the
 *            instrument.
 *
 * Returns:
 *  0 - on success
 */
vx_EXPORT int vxi11_close_device(VXI11_CLINK *clink, const char *address);


/* Function: vxi11_send
 *
 * Send data to an instrument.
 *
 * Parameters:
 *  clink - a valid VXI11_CLINK pointer.
 *  cmd   - the command to send as an array of bytes
 *  len   - the length of cmd
 *
 * Returns:
 *  0                      - on success
 *  1                      - on out of memory
 *  -VXI11_NULL_WRITE_RESP - on send timeout (retry is acceptable)
 */
vx_EXPORT int vxi11_send(VXI11_CLINK *clink, const char *cmd, size_t len);


/* Function: vxi11_send_printf
 *
 * Send data to an instrument. Convenience function when sending text.
 *
 * Parameters:
 *  clink  - a valid VXI11_CLINK pointer.
 *  format - printf style format specifier.
 *
 * Returns:
 *  0                      - on success
 *  1                      - on out of memory
 *  -VXI11_NULL_WRITE_RESP - on send timeout (retry is acceptable)
 */
vx_EXPORT int vxi11_send_printf(VXI11_CLINK *clink, const char *format, ...);


/* Function: vxi11_receive
 *
 * Receive data from an instrument. Uses VXI11_READ_TIMEOUT as the timeout.
 *
 * Parameters:
 *  clink  - a valid VXI11_CLINK pointer.
 *  buffer - valid memory location in which to receive data.
 *  len    - number of bytes requested - buffer must be at least this large
 *
 * Returns:
 *  Number of bytes read  - on success
 *  -VXI11_NULL_READ_RESP - on timeout
 *  -100                  - on "buffer too small"
 */
vx_EXPORT ssize_t vxi11_receive(VXI11_CLINK *clink, char *buffer, size_t len);


/* Function: vxi11_receive_timeout
 *
 * Receive data from an instrument. Own timeout can be specified.
 *
 * Parameters:
 *  clink   - a valid VXI11_CLINK pointer.
 *  buffer  - valid memory location in which to receive data.
 *  len     - number of bytes requested - buffer must be at least this large
 *  timeout - the number of milliseconds to wait before returning if no data is
 *            received.
 *
 * Returns:
 *  Number of bytes read  - on success
 *  -VXI11_NULL_READ_RESP - on timeout
 *  -100                  - on "buffer too small"
 */
vx_EXPORT ssize_t vxi11_receive_timeout(VXI11_CLINK *clink, char *buffer, size_t len, unsigned long timeout);


/* Function: vxi11_send_data_block
 *
 * Utility function to send a command and a data block.
 *
 * Parameters:
 *  clink  - a valid VXI11_CLINK pointer.
 *  cmd    - text command to send
 *  buffer - data to send
 *  len    - length of buffer
 *
 * Returns:
 *  0                      - on success
 *  1                      - on out of memory
 *  -VXI11_NULL_WRITE_RESP - on send timeout (retry is acceptable)
 */
vx_EXPORT int vxi11_send_data_block(VXI11_CLINK *clink, const char *cmd, char *buffer, size_t len);


/* Function: vxi11_receive_data_block
 *
 * This function reads a response in the form of a definite-length block, such
 * as when you ask for waveform data. The data is returned in the following
 * format:
 *   #800001000<1000 bytes of data>
 *   ||\______/
 *   ||    |
 *   ||    \---- number of bytes of data
 *   |\--------- number of digits that follow (in this case 8, with leading 0's)
 *   \---------- always starts with #
 *
 * Parameters:
 *  clink   - a valid VXI11_CLINK pointer.
 *  buffer  - valid memory location in which to receive data.
 *  len     - number of bytes requested - buffer must be at least this large
 *  timeout - the number of milliseconds to wait before returning if no data is
 *            received.
 *
 * Returns:
 *  Number of bytes read  - on success
 *  -VXI11_NULL_READ_RESP - on timeout
 *  -100                  - on "buffer too small"
 */
vx_EXPORT ssize_t vxi11_receive_data_block(VXI11_CLINK *clink, char *buffer, size_t len, unsigned long timeout);


/* Function: vxi11_send_and_receive
 *
 * Utility function to send a command and receive a response.
 *
 * Parameters:
 *  clink   - a valid VXI11_CLINK pointer.
 *  cmd     - text command to send
 *  buffer  - valid memory location in which to receive data.
 *  len     - number of bytes requested - buffer must be at least this large
 *  timeout - the number of milliseconds to wait before returning if no data is received.
 *
 * Returns:
 *   0 - on success
 *  -1 - on write failure
 *  -2 - on read failure
 */
vx_EXPORT int vxi11_send_and_receive(VXI11_CLINK *clink, const char *cmd, char *buf, size_t len, unsigned long timeout);


/* Function: vxi11_obtain_long_value
 *
 * Utility function to receive a long integer. Uses VXI11_READ_TIMEOUT as the
 * timeout.
 *
 * Parameters:
 *  clink - a valid VXI11_CLINK pointer.
 *  cmd   - text command to send
 *
 * Returns:
 *  long integer value, or 0 on failure
 */
vx_EXPORT long vxi11_obtain_long_value(VXI11_CLINK *clink, const char *cmd);


/* Function: vxi11_obtain_long_value_timeout
 *
 * Utility function to receive a long integer, with a user specified timeout.
 *
 * Parameters:
 *  clink   - a valid VXI11_CLINK pointer.
 *  cmd     - text command to send
 *  timeout - the number of milliseconds to wait before returning if no data is
 *            received.
 *
 * Returns:
 *  long integer value, or 0 on failure
 */
vx_EXPORT long vxi11_obtain_long_value_timeout(VXI11_CLINK *clink, const char *cmd, unsigned long timeout);


/* Function: vxi11_obtain_double_value_timeout
 *
 * Utility function to receive a double precision float Uses VXI11_READ_TIMEOUT
 * as the timeout.
 *
 * Parameters:
 *  clink - a valid VXI11_CLINK pointer.
 *  cmd   - text command to send
 *
 * Returns:
 *  double value, or 0.0 on failure
 */
vx_EXPORT double vxi11_obtain_double_value(VXI11_CLINK *clink, const char *cmd);


/* Function: vxi11_obtain_double_value_timeout
 *
 * Utility function to receive a double precision float, with a user specified
 * timeout.
 *
 * Parameters:
 *  clink   - a valid VXI11_CLINK pointer.
 *  cmd     - text command to send
 *  timeout - the number of milliseconds to wait before returning if no data is
 *            received.
 *
 * Returns:
 *  double value, or 0.0 on failure
 */
vx_EXPORT double vxi11_obtain_double_value_timeout(VXI11_CLINK *clink, const char *cmd, unsigned long timeout);

#ifdef __cplusplus
}
#endif

#endif
