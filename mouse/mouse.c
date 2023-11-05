/*
// Q68 PS/2 mouse driver for the pointer environment
//
// Copyright (C) 2023 Peter Graf
//
// This program is free software, you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY, without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program, if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

/* Compile with C68 and commandline options: -O -screspr.o  */

#include <qdos.h>

#define MOUSE_CODE (* (volatile unsigned char*) 0x1C160)
#define MOUSE_UNLOCK (* (volatile unsigned char*) 0x1C164)
#define MOUSE_STATUS (* (volatile unsigned char*) 0x1C168)

#define ReadLong(a) (* (volatile unsigned long*) (a))
#define ReadByte(a) (* (volatile unsigned char*) (a))
#define ReadWord(a) (* (volatile unsigned short*) (a))
#define WriteByte(a,d) (* (volatile unsigned char*) (a) = (d))
#define WriteWord(a,d) (* (volatile unsigned short*) (a) = (d))

static unsigned long ScrDrvAddr = 0;
static unsigned char intellimouse = 0;
static unsigned char stat;
static short x, y;
static signed char dx, dy, dz;
static short xsize = 512;
static short ysize = 256;

/* Write absolute XY mouse position into PE, magic adress offsets like UQLX */
void SetMouseXY(short x, short y)
{
  WriteWord(ScrDrvAddr+0x20, x);
  WriteWord(ScrDrvAddr+0x22, y);
  WriteByte(ScrDrvAddr+0x52-0x18, 0);
  WriteByte(ScrDrvAddr+0x8f-0x18, 0);
}

/* Write mouse button state into PE */
/* No middle button support yet */
void SetMouseButtons(unsigned char stat)
{
  WriteByte(ScrDrvAddr+0xaf-0x18, stat & 0x03);
  WriteByte(ScrDrvAddr+0x8c-0x18, 0);
}

/* Read movement data from PS/2 mouse in stream mode
// Requires previously initialized mouse (done by bootloader)
//
// Polling routine, does not wait if no new movement data available
//
// Receiving further mouse data is held back until next call,
// so a slow polling rate of about 50 Hz should be fine
//
// Note: It seems wrong information that movement values are 9 bit,
// where the most significant bit appears as sign bit in the status byte!
//
// X and Y movement appeared 8 bit signed, the sign bit in status was
// only a copy of the MSB
//
// Output data (must be kept between calls):
// stat  Bit 7..3 not needed (Y overflow,   X overflow, Y sign bit, X sign bit, 1)
//       Bit 2: Middle button, Bit 1:Right Button, Bit 0:Left Button
// dx    Relative X movement of mouse since last report (8 bit signed)
// dy    Relative Y movement of mouse since last report (8 bit signed)
//
// Return value:
// 0=More bytes need to be received, output data is invalid
// 1=Movement Data Packet complete, output data for stat, dx, dy is valid */

unsigned char GetPS2MouseMove(void)
{
  static short stream_state = 0;

  if(MOUSE_STATUS & 0x01) /* Bit 0 high: Mouse code received */
  {
    if (stream_state == 0)
      stat = MOUSE_CODE;
    else if (stream_state == 1)
      dx = MOUSE_CODE;
    else if (stream_state == 2)
      dy = MOUSE_CODE;
    else /* stream_state == 3 */
      dz = MOUSE_CODE;
    
    MOUSE_UNLOCK = 0x01; /* Unlock PS/2 receive */
    
    /* Bit 3 of first mouse byte must always be set. If not, retry */
    if ((stream_state == 0) && ((stat & 0x08) != 0x08))
      stream_state = 0;
    else if ((stream_state < 2) || (intellimouse && (stream_state < 3)))
      stream_state++;
    else
    {
      stream_state = 0;
      return 1; /* Movement data complete */
    }
  }
  return 0; /* More bytes required to complete packet from host */
}

#pragma forcedsave=all
/* Should force the compiler to save registers on function entry and restore when
   leaving the function. Uncertain if it really works */
void PolledTask(void)
{
  if (GetPS2MouseMove())
  {
    /* printf("dx=%i dy=%i\n", (int)dx, (int)dy); */
    SetMouseButtons(stat);

    x = ReadWord(ScrDrvAddr+0x20) + dx;
    if (x <= 1) x = 1;
    if (x >= xsize) x = xsize - 1;

    y = ReadWord(ScrDrvAddr+0x22) - dy;
    if (y <= 1) y = 1;
    if (y >= ysize) y = ysize - 1;

    SetMouseXY(x, y);

    /* Magic so update is seen by PE */
    WriteByte(ScrDrvAddr+0x16, ReadByte(ScrDrvAddr+0x16)+1);
  }
}
#pragma forcedsave=none

static QL_LINK_t link_polled_task[2] = {0, PolledTask};

void main(void)
{
  unsigned long SysVarAddr;
  long VersionCode;
  
  if (MOUSE_STATUS & 0x10)
    intellimouse = 1;

  /* Should check for Pointer Environment here, but required C68 libs buggy */

  /* ScrDrvAddr = ReadLong(ReadLong(ReadLong(0x28078))+4); */
  mt_inf((char**)&SysVarAddr, &VersionCode);
  ScrDrvAddr = ReadLong(ReadLong(ReadLong(SysVarAddr+0x78))+4);

  /* mt_lpoll(link_polled_task); */
  
  mt_lschd(link_polled_task);

  /* We have no interrupt line on the QL ROM port */
  /* mt_lxint(link_polled_task); */ 

  /* Enabling mouse interrupt would look like this on a Q68 */
  /* MOUSE_STATUS = MOUSE_STATUS | 0x80; */
  return;
}
