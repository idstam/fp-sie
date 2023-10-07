unit USieClasses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Nullable;

type
  TSieObjectBase = class;
  TSieDimensionBase = class;
  TSieVoucherRowBase = class;

  THashSetString = specialize THashSet<string>;
  THashSetSieDimensionBase = specialize THashSet<TSieDimensionBase>;

  TNullableCurrency = specialize TNullable<Currency>;
  TNullableInteger = specialize TNullable<integer>;

  TListSieObjectBase = specialize TList<TSieObjectBase>;
  TListSieVoucherRowBase = specialize TList<TSieVoucherRowBase>;

  TDictStringSieObjectBase = specialize TDictionary<string, TSieObjectBase>;

  TSieAccountBase = class
  private
  public
    Number: string;
    Name: string;
    AccUnit: string;
    AccType: string;
    SRU: THashSetString;
  end;

  TSieCompanyBase = class
  private

  public
    //#BKOD
    SNI: integer;

    //#FNAMN
    Name: string;

    //#FNR
    Code: string;

    //#FTYP
    OrgType: string;

    //#ORGNR
    OrgIdentifier: string;

    //#ADRESS
    Contact: string;
    Street: string;
    ZipCity: string;
    Phone: string;

  end;

  TSieDimensionBase = class
  private
    _parent: TSieDimensionBase;
  public
    Number: string;
    Name: string;
    IsDefault: boolean;
    SubDim: THashSetSieDimensionBase;
    Objects: TDictStringSieObjectBase;
  end;

  TSieObjectBase = class
  private
  public
    Dimension: TSieDimensionBase;
    Number: string;
    Name: string;
  end;

  TSiePeriodValueBase = class
  private
  public
    Account : TSieAccountBase;
    YearNr: integer;
    Period: integer;
    Amount: Currency;
    Quantity: TNullableCurrency;
    Objects: TListSieObjectBase;
    Token: string;
    function ToVoucherRow(): TSieDimensionBase; virtual; abstract;
  end;

  TSieBookingYearBase = class
    ID: integer;
    StartDate:  TNullableInteger;
    EndDate: TNullableInteger;
  end;

  TSieVoucherRowBase = class
  private
  public
    Account: TSieAccountBase;
    Objects: TListSieObjectBase;
    Amount: Currency;
    RowDate:integer;
    Text: string;
    Quantity: TNullableCurrency;
    CreatedBy : string;
    Token: string;
  end;

  TSieVoucherBase = class
  private
  public
    Series: string;
    Number: string;
    VoucherDate: integer;
    Text: string;
    CreatedDate: integer;
    CreatedBy: string;
    Token: string;
    Rows: TListSieVoucherRowBase;
    constructor Create(); virtual;abstract;
  end;

  TSieError = class
    public
      Message:string
  end;

implementation

end.
