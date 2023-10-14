unit USieCallbacks;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, USieClasses;

type
  TSieCallbackBase = class
  public
    procedure Line(aLine: string);
    procedure Error(aError: TSieError);
    procedure IB(aPeriodValue: TSiePeriodValue);
    procedure UB(aPeriodValue: TSiePeriodValue);
    procedure OIB(aPeriodValue: TSiePeriodValue);
    procedure OUB(aPeriodValue: TSiePeriodValue);
    procedure PSALDO(aPeriodValue: TSiePeriodValue);
    procedure PBUDGET(aPeriodValue: TSiePeriodValue);
    procedure RES(aPeriodValue: TSiePeriodValue);
    procedure VOUCHER(aVoucher: TSieVoucher);
  end;

implementation

procedure TSieCallbackBase.Line(aLine: string);
begin

end;

procedure TSieCallbackBase.Error(aError: TSieError);
begin

end;

procedure TSieCallbackBase.IB(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.UB(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.OIB(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.OUB(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.PSALDO(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.PBUDGET(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.RES(aPeriodValue: TSiePeriodValue);
begin

end;

procedure TSieCallbackBase.VOUCHER(aVoucher: TSieVoucher);
begin

end;


end.
