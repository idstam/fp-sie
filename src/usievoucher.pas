unit USieVoucher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieClasses;

type
  TSieVoucherRow = class(TSieVoucherRowBase)

  end;

  TSieVoucher = class(TSieVoucherBase)
  public
    constructor Create(); override;
  end;

implementation

constructor TSieVoucher.Create();
begin
  self.Rows := TListSieVoucherRowBase.Create();
end;

end.

