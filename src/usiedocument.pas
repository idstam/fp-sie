unit USieDocument;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,USieClasses, Generics.Collections;

type
  TSieDocument = class
  private
    _fileName:string;
  public
    DateFormat:String;
    FNAMN:TSieCompany;
    KONTO: specialize  TDictionary<string, TSieAccount>;
    DIM: specialize  TDictionary<string, TSieDimension>;
    OIB: specialize TList<SiePeriodValue>;
    OUB: specialize TList<SiePeriodValue>;
    PSALDO: specialize TList<SiePeriodValue>;
    PBUDGET: specialize TList<SiePeriodValue>;
    PROGRAM: specialize TList<string>;
    RAR: specialize  TDictionary<string, TSieBookingYear>;
    IB: specialize TList<TSiePeriodValue>;
    UB: specialize TList<TSiePeriodValue>;
    RES: specialize TList<TSiePeriodValue>;
    VER: specialize TList<TSieVoucher>;
    ValidationExceptions: specialize TList<TSieErrors>;
    //CRC

    constructor Create();
    procedure ReadDocument(fileName: string);
  published

  end;
implementation
  constructor TSieDocument.Create();
  begin
    self.DateFormat:='yyyyMMdd';
  end;

  procedure TSieDocument.ReadDocument(fileName: string);
  begin
    self._fileName := fileName;
  end;

end.

