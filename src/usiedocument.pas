unit USieDocument;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,USieClasses, Generics.Collections, USieCompany, USieAccount, USieDimension, USiePeriodvalue, USieBookingYear, USieVoucher;

type



  TDictStringSieAccount = specialize TDictionary<string, TSieAccount>;
  TDictStringSieDimension = specialize TDictionary<string, TSieDimension>;
  TDictStringSieBookingYear = specialize TDictionary<string, TSieBookingYear>;
  TListSiePeriodValue = specialize TList<TSiePeriodValue>;
  TListSieVoucher = specialize TList<TSieVoucher>;
  TListSieError = specialize TList<TSieError>;


  TSieDocument = class
  private
    _fileName:string;
    procedure initializeFields();
  public
    DateFormat:String;
    FNAMN:TSieCompany;
    KONTO: TDictStringSieAccount;
    DIM: TDictStringSieDimension;
    OIB: TListSiePeriodValue;
    OUB: TListSiePeriodValue;
    PSALDO: TListSiePeriodValue;
    PBUDGET: TListSiePeriodValue;
    //#PROGRAM
    PROGRAMS: TStringList;
    RAR: TDictStringSieBookingYear;
    IB: TListSiePeriodValue;
    UB: TListSiePeriodValue;
    RES: TListSiePeriodValue;
    VER: TListSieVoucher;
    ValidationExceptions: TListSieError;
    //CRC

    constructor Create();
    procedure ReadDocument(aFileName: string);
    class function GetSieVersion(aFileName: string):integer; static;
  published

  end;
implementation
  uses USieDataItem;

  constructor TSieDocument.Create();
  begin
    self.DateFormat:='yyyyMMdd';

  end;
  procedure TSieDocument.initializeFields();
  begin
    self.FNAMN := TSieCompany.Create();
    self.KONTO := TDictStringSieAccount.Create();
    self.DIM := TDictStringSieDimension.Create();
    self.OIB := TListSiePeriodValue.Create();
    self.OUB := TListSiePeriodValue.Create();
    self.PSALDO := TListSiePeriodValue.Create();
    self.PBUDGET := TListSiePeriodValue.Create();
    self.PROGRAMS := TStringList.Create();
    self.RAR := TDictStringSieBookingYear.Create();
    self.IB := TListSiePeriodValue.Create();
    self.UB := TListSiePeriodValue.Create();
    self.RES := TListSiePeriodValue.Create();
    self.VER := TListSieVoucher.Create();
    self.ValidationExceptions := TListSieError.Create();

    self.DIM.AddOrSetValue('1',  TSieDimension.Create('1', 'Resultatenhet', true));
    self.DIM.AddOrSetValue('2',  TSieDimension.Create('2', 'Kostnadsbärare', DIM['1'], true ));
    self.DIM.AddOrSetValue('3',  TSieDimension.Create('3', 'Reserverat', true ));
    self.DIM.AddOrSetValue('4',  TSieDimension.Create('4', 'Reserverat', true ));
    self.DIM.AddOrSetValue('5',  TSieDimension.Create('5', 'Reserverat', true ));
    self.DIM.AddOrSetValue('6',  TSieDimension.Create('6', 'Projekt', true ));
    self.DIM.AddOrSetValue('7',  TSieDimension.Create('7', 'Anställd', true ));
    self.DIM.AddOrSetValue('8',  TSieDimension.Create('8', 'Kund', true ));
    self.DIM.AddOrSetValue('9',  TSieDimension.Create('9', 'Leverantör', true ));
    self.DIM.AddOrSetValue('10', TSieDimension.Create('10', 'Faktura', true ));
    self.DIM.AddOrSetValue('11', TSieDimension.Create('11', 'Reserverat', true ));
    self.DIM.AddOrSetValue('12', TSieDimension.Create('12', 'Reserverat', true ));
    self.DIM.AddOrSetValue('13', TSieDimension.Create('13', 'Reserverat', true ));
    self.DIM.AddOrSetValue('14', TSieDimension.Create('14', 'Reserverat', true ));
    self.DIM.AddOrSetValue('15', TSieDimension.Create('15', 'Reserverat', true ));
    self.DIM.AddOrSetValue('16', TSieDimension.Create('16', 'Reserverat', true ));
    self.DIM.AddOrSetValue('17', TSieDimension.Create('17', 'Reserverat', true ));
    self.DIM.AddOrSetValue('18', TSieDimension.Create('18', 'Reserverat', true ));
    self.DIM.AddOrSetValue('19', TSieDimension.Create('19', 'Reserverat', true ));

  end;

  procedure TSieDocument.ReadDocument(aFileName: string);
  begin
    self._fileName := afileName;
  end;

  class function TSieDocument.GetSieVersion(aFileName: string):integer; static;
  var
    F:Text;
    line: string;
    di: TSieDataItem;
    ret:integer;
  begin
       ret := -1;
       AssignFile(F, aFileName);
       Reset(F);
       while not eof(F) do
       begin
            ReadLn(F, line);
            if line.StartsWith('#SIETYP') then
            begin
              di := TSieDataItem.Create(line);
              ret := di.GetInt(0);
              break;
            end;
       end;
       exit(ret);
  end;

end.

