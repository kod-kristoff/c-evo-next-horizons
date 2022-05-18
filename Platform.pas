unit Platform;

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}Unix,{$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs;

function NowPrecise: TDateTime;


implementation

{$IFDEF WINDOWS}
var
  PerformanceFrequency: Int64;
{$ENDIF}

var
  NowPreciseLock: TCriticalSection;

function NowPrecise: TDateTime;
var
  {$IFDEF UNIX}T: TimeVal;{$ENDIF}
  {$IFDEF WINDOWS}TimerValue: Int64;{$ENDIF}
begin
//  Result := Now;
  //try
    //NowPreciseLock.Acquire;
    {$IFDEF WINDOWS}
    QueryPerformanceCounter(TimerValue);
    //Result := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)) * 1000) // an alternative Win32 timebase
    Result := TimerValue / PerformanceFrequency;
    {$ENDIF}

    {$IFDEF UNIX}
    fpgettimeofday(@t, nil);
     // Build a 64 bit microsecond tick from the seconds and microsecond longints
    Result := t.tv_sec + t.tv_usec / 1000000;
    {$ENDIF}

    Result := Result * OneSecond;
    //Result := (Trunc(Now / OneSecond) + Frac(Result)) * OneSecond;
  //finally
    //NowPreciseLock.Release;
  //end;
end;

initialization

{$IFDEF WINDOWS}
QueryPerformanceFrequency(PerformanceFrequency);
{$ENDIF}
NowPreciseLock := TCriticalSection.Create;

finalization

FreeAndNil(NowPreciseLock);

end.

