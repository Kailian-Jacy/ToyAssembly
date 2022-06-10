/* 编译及运行步骤:
把此文件复制到xp虚拟机d:\tc中
运行tc后:
Alt+F选择File->Load->hexfile.c
Alt+C选择Compile->Compile to OBJ 编译
Alt+C选择Compile->Line EXE file 连接
Alt+R选择Run->Run 运行

    或

把此文件复制到Bochs虚拟机的c:\tc中, 
运行Bochs虚拟机
c:
cd \tc
tc
Alt+F选择File->Load->hexfile.c
Alt+C选择Compile->Compile to OBJ 编译
Alt+C选择Compile->Line EXE file 连接
Alt+R选择Run->Run 运行
 */
#include <stdio.h>
#include <stdlib.h>
#include <bios.h>
#define PageUp   0x4900
#define PageDown 0x5100
#define Home     0x4700
#define End      0x4F00
#define F5       0x3F00
#define BKSPACE  0x0E08
#define ENTER    0x1C0D
#define Esc      0x011B

typedef unsigned char byte;
typedef unsigned short int word;
typedef unsigned long int dword;

void char2hex(byte xx, byte s[]) /* 把8位数转化成16进制格式 */
{
   char t[] = "0123456789ABCDEF";
   s[0] = t[(xx >> 4) & 0x0F]; /* 高4位 */
   s[1] = t[xx & 0x0F];        /* 低4位 */
}

void long2hex(dword offset, byte s[]) /* 把32位数转化成16进制格式 */
{
   int i;
   byte xx;
   for(i=0; i<4; i++)
   {
      offset = _lrotl(offset, 8); /* 循环左移8位, 把高8位移到低8位 */
      xx = offset & 0xFF;         /* 高24位置0, 保留低8位 */
      char2hex(xx, &s[i*2]);      /* 把8位数转化成16进制格式 */
   }
}

long hex2long(byte s[]) /* 16进制字符串转化成32位数 */
{
   char t[] = "0123456789ABCDEF";
   int i, j, n;
   dword y=0;
   n = strlen(s);
   for(i=0; i<n; i++)
   {
      for(j=0; j<sizeof(t)-1; j++)
      {
         if(t[j] == s[i])
            break;
      }
      y = (y << 4) | j;
   }
   return y;
}

void show_this_row(int row, dword offset, byte buf[], int bytes_on_row)
{  /* 显示当前一行:   行号       偏移    数组首地址      当前行字节数 */
   char far *vp = (char far *)0xB8000000;
   char s[]= 
      "00000000: xx xx xx xx|xx xx xx xx|xx xx xx xx|xx xx xx xx  ................";
   /*  |         |                                                |
       |         |                                                |
       00        10                                               59
       上面一行的3个两位数是竖线对应位置元素的下标;
       数组s的内容就是每行的输出格式:
       其中左侧8个0表示当前偏移地址;
       其中xx代表16进制格式的一个字节;
       其中s[59]开始共16个点代表数组buf各个元素对应的ASCII字符。
    */
   char pattern[] = 
      "00000000:            |           |           |                             ";
   int i;
   strcpy(s, pattern);
   long2hex(offset, s); /* 把32位偏移地址转化成16进制格式填入s左侧8个'0'处 */
   for(i=0; i<bytes_on_row; i++) /* 把buf中各个字节转化成16进制格式填入s中的xx处 */
   {
      char2hex(buf[i], s+10+i*3);
   }
   for(i=0; i<bytes_on_row; i++) /* 把buf中各个字节填入s右侧小数点处 */
   {
      s[59+i] = buf[i];
   }
   vp = vp + row*80*2;           /* 计算row行对应的视频地址 */
   for(i=0; i<sizeof(s)-1; i++)  /* 输出s */
   {
      vp[i*2] = s[i];
      if(i<59 && s[i] == '|')    /* 把竖线的前景色设为高亮度白色 */
         vp[i*2+1] = 0x0F;
      else                       /* 其它字符的前景色设为白色 */
         vp[i*2+1] = 0x07;
   }
}

void clear_this_page(void)       /* 清除屏幕0~15行 */
{
   char far *vp = (char far *)0xB8000000;
   int i, j;
   for(i=0; i<16; i++)           /* 汇编中可以使用rep stosw填入80*16个0020h */
   {
      for(j=0; j<80; j++)
      {
         *(vp+(i*80+j)*2) = ' ';
         *(vp+(i*80+j)*2+1) = 0;
      }
   }
}

void show_this_page(byte buf[], dword offset, int bytes_in_buf)
{  /* 显示当前页:   数组首地址       偏移        当前页字节数 */
   int i, rows, bytes_on_row;
   clear_this_page();
   rows = (bytes_in_buf + 15) / 16; /* 计算当前页的行数, 每16字节为一行 */
   for(i=0; i< rows; i++)
   {
      bytes_on_row = (i == rows-1) ? (bytes_in_buf - i*16) : 16; /* 当前行的字节数 */
      show_this_row(i, offset+i*16, &buf[i*16], bytes_on_row); /* 显示这一行 */
   }
}

dword get_offset(void)
{
   char far *vp = (char far *)0xB8000000, far *p;
   int x = (80 - 12)/2, y = (25 - 3)/2;
   int i, j, n;
   word key;
   char key_map[] = "0123456789ABCDEF";
   byte input[9];
   char box[3][13] = 
   {
      "+----------+",
      "|          |",
      "+----------+"
   };
   char buf[3][12*2];
   p = vp + (y*80+x)*2;
   for(i=0; i<3; i++) /* 保存屏幕上弹框区域已显示的信息到buf中 */
   {
      for(j=0; j<12*2; j++)
      {
         buf[i][j] = p[j];
      }
      p += 80*2;
   }

   p = vp + (y*80+x)*2;
   for(i=0; i<3; i++)
   {
      for(j=0; j<12; j++)
      {
         p[j*2] = box[i][j];
         p[j*2+1] = 0x17; /* back color = blue, front color = white */
      }
      p += 80*2;
   }

   p = vp + ((y+1)*80 + (x+1))*2; /* p->输入框内文字 */
   n = 0; /* 已输入字符个数 */
   do
   {
      key = bioskey(0);
      if(key == BKSPACE)
      {
         if(n == 0)
            continue;
         p[(n-1)*2] = ' '; /* 用空格覆盖此字符 */
         p[(n-1)*2+1] = 0x17;         
         n--;
         continue;
      }
      if(key == ENTER)
      {
         input[n] = '\0';
         continue;
      }
      key &= 0xFF;
      if(key >= 'a' && key <= 'f')
         key -= 0x20; /* 转成大写 */
      for(i=0; i<sizeof(key_map)-1; i++)
      {
         if(key == key_map[i])
            break;
      }
      if(i == sizeof(key_map)-1)
         continue; /* 不许输入非十六进制字符 */
      if(n == 8)
         continue; /* 不能超过8个字符 */
      input[n] = key;
      p[n*2] = key; /* 显示此字符 */
      p[n*2+1] = 0x17;
      n++;
   } while(key != ENTER);

   p = vp + (y*80+x)*2;
   for(i=0; i<3; i++) /* 恢复屏幕上弹框区域的信息 */
   {
      for(j=0; j<12*2; j++)
      {
         p[j] = buf[i][j];
      }
      p += 80*2;
   }
   return hex2long(input);
}

main()
{
   char filename[100];
   char buf[256];
   int  key, bytes_in_buf;
   dword file_size, offset, n, old_offset;
   FILE *fp;
   puts("Please input filename:");
   gets(filename); /* 输入文件名; 汇编中可以调用int 21h/AH=0Ah功能 */
   fp = fopen(filename, "rb");  /* 以二进制只读方式打开文件 
                                   汇编对应调用: 
                                   mov ah, 3Dh
                                   mov al, 0
                                   mov dx, offset filename
                                   mov ds, seg filename
                                   int 21h; CF=0 on success, AX=handle
                                   mov handle, ax; handle为dw类型的变量
                                 */
   if(fp == NULL)               /* 汇编中可以通过检查CF==0来判断上述打开文件有否成功 */
   {
      puts("Cannot open file!");
      exit(0); /* 汇编对应调用: 
                  mov ah, 4Ch
                  mov al, 0
                  int 21h
                */
   }
   fseek(fp, 0, SEEK_END); /* 以EOF为起点移动文件指针, 移动距离为0, 即文件指针仍旧
                              停留在EOF位置.
                              汇编对应调用:
                              mov ah, 42h
                              mov al, 2; SEEK_END, 表示以EOF为起点移动文件指针
                              mov bx, handle
                              mov cx, 0; \ 移动距离为cx:dx
                              mov dx, 0; / 
                              int 21h  ; 返回dx:ax=文件长度  
                              mov word ptr file_size[2], dx
                              mov word ptr file_size[0], ax
                            */
   file_size = ftell(fp);  /* 汇编不需要调用跟此函数相关的中断, 因为int 21h/AH=42h已经
                              获得文件长度.
                            */
   fseek(fp, 0, SEEK_SET); /* 重新移动文件指针到偏移0处, 即文件内容的首字节处 */
                           /* 汇编调用:
                              mov ah, 42h
                              mov al, 0; SEEK_SET, 以文件内容的首字节为起点移动文件指针
                              mov bx, handle
                              mov cx, 0;\ 移动距离 = 0
                              mov dx, 0;/
                              int 21h
                            */
   offset = 0;
   do
   {
      n = file_size - offset;
      if(n >= 256)
         bytes_in_buf = 256;
      else
         bytes_in_buf = n;
      fseek(fp, offset, SEEK_SET);  /* 移动文件指针;
                                       汇编对应调用:
                                       mov ah, 42h
                                       mov al, 0
                                       mov bx, handle
                                       mov cx, word ptr offset[2]; \cx:dx一起构成
                                       mov dx, word ptr offset[0]; /32位值=offset
                                       int 21h
                                     */
      bytes_in_buf = fread(buf, 1, bytes_in_buf, fp); 
                                    /* 读取文件中的bytes_in_buf个字节到buf中 
                                       汇编对应调用:
                                       mov ah, 3Fh
                                       mov bx, handle
                                       mov cx, bytes_in_buf
                                       mov dx, offset buf; ds:dx->buf
                                       mov ds, seg buf   ;
                                       int 21h; CF=0 on success, AX=bytes actually read
                                     */
      show_this_page(buf, offset, bytes_in_buf);
      key = bioskey(0); /* 键盘输入;
                           汇编对应调用:
                           mov ah, 0
                           int 16h
                         */
      switch(key)
      {
      case PageUp:
         if(offset < 256)
            offset = 0;
         else
            offset = offset - 256;
         break;
      case PageDown:
         if(offset + 256 < file_size)
            offset = offset + 256;
         break;
      case Home:
         offset = 0;
         break;
      case F5:
         old_offset = offset;
         offset = get_offset();  /* 弹框输入offset */
         if(offset >= file_size) /* 输入的offset不能超出文件范围 */
            offset = old_offset;         
         break;
      case End:
         offset = file_size % 256 == 0 ? file_size - 256 : file_size - file_size % 256;
         break;
      }
   } while(key != Esc);
   fclose(fp); /* 关闭文件; 
                  汇编对应调用:
                  mov ah, 3Eh
                  mov bx, handle
                  int 21h
                */
}
