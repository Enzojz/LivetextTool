namespace Livetext
open System.Runtime.InteropServices
open System

module Import = 
  [<Struct>]
  type ABCFLOAT = {
      abcfA : float32;
      abcfB : float32;
      abcfC : float32;
    }
  
  [<Struct>]
  type KERNINGPAIR = {
    wFirst : uint16;
    wSecond : uint16;
    iKernelAmount : int
  }
  
  [<DllImport("gdi32.dll", SetLastError = true)>]
  extern uint32 GetKerningPairsW(IntPtr hdc, uint32 nPairs, [<In>][<Out>] KERNINGPAIR[] pairs);
  
  [<DllImport("gdi32.dll")>]
  extern IntPtr SelectObject(IntPtr hdc, IntPtr hObject);

  [<DllImport("gdi32.dll")>]
  extern bool GetCharABCWidthsFloatW(IntPtr hdc, uint32 iFirstChar, uint32 iLastChar, [<In>][<Out>] ABCFLOAT[] lpABCF);
