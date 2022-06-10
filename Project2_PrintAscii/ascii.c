/* 编译步骤:
(1.1) 把此文件复制到xp虚拟机d:\tc中
双击桌面上的tc图标运行tc
      或
(1.2) 把此文件复制到Bochs虚拟机的硬盘镜像dos.img中的c:\tc文件夹内
(dos.img用主页上的工具软件10.71.45.100/bhh/winimage9.zip打开), 运
行Bochs虚拟机
cd \tc
tc

(2)
Alt+F选择File->Load->ascii.c
Alt+C选择Compile->Compile to OBJ 编译
Alt+C选择Compile->Line EXE file 连接
Alt+R选择Run->Run 运行
Alt+R选择Run->User Screen 查看结果
*/
#include <dos.h>
#include <bios.h>
main()
{
   unsigned char a=0, hex[3];
   char far *p = (char far *)0xB8000000;
   int i, j, k;
   _AX = 0x0003;
   geninterrupt(0x10);     /* 设置视频模式为80*25文本模式, 同时起到清屏效果 */
   for(j=0; j<11; j++)     /* 共11列 */
   {
      p = (char far *)0xB8000000;
      p += j*7*2;          /* 计算第j列第0行的地址, 注意各列之间相距7个字符(含空格) */
                           /* 汇编中可以根据前后两列首地址相差14字节的规律,用加法计算
                              出下一列的地址
                            */
      for(i=0; i<25; i++)  /* 每列输出25个ASCII码, 即每列有25行 */
      {
         *p = a;           /* 输出当前ASCII字符 */
         *(p+1) = 0x0C;    /* 背景为黑色, 前景为高亮度红色 */
         sprintf(hex, "%02X", a);
                           /* 汇编中可以利用上课讲过的循环左移4位的方法把a的值转化
                              成2位十六进制并保存到数组hex中
                            */
         for(k=0; k<2; k++)/* 输出2位十六进制数 */
         {
            *(p+2+k*2) = hex[k];
            *(p+2+k*2+1) = 0x0A; /* 背景为黑色, 前景为高亮度绿色 */
         }
         a++;
         if(a==0)          /* 若a从0xFF变成0x00, 表示256个ASCII码已穷尽 */
            goto done;
         p += 160;         /* p指向第j列的第i+1行 */
      }
   }
   done:
   bioskey(0);             /* 汇编中可以用mov ah,0; int 16h;代替此函数调用 */
}
