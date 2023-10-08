unit USieVoucher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieClasses;

type
  TSieVoucherRow = class(TSieVoucherRow)

  end;

  TSieVoucher = class(TSieVoucher)
  public
    constructor Create(); override;
  end;

implementation

constructor TSieVoucher.Create();
begin
  self.Rows := TListSieVoucherRow.Create();
end;

end.

