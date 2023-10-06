unit USieClasses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Nullable;

type
  TSieObjectBase = class;

  TSieAccountBase = class
  private
  public
    Number: string;
    Name: string;
    AccUnit: string;
    AccType: string;
    SRU: specialize THashSet<string>;
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
    SubDim: specialize THashSet<TSieDimensionBase>;
    Objects: specialize TDictionary<string, TSieObjectBase>;

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
    Quantity: specialize TNullable<Currency>;
    Objects: specialize TList<TSieObjectBase>;
    Token: string;
    function ToVoucherRow(): TSieDimensionBase; virtual; abstract;
  end;

  TSieBookingYearBase = class
    ID: integer;
    StartDate:  specialize TNullable<integer>;
    EndDate: specialize TNullable<integer>;
  end;

  TSieVoucherRowBase = class
  private
  public
    Account: TSieAccountBase;
    Objects: specialize TList<TSieObjectBase>;
    Amount: Currency;
    RowDate:integer;
    Text: string;
    Quantity: specialize TNullable<Currency>;
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
    Rows: specialize TList<TSieVoucherRowBase>;
    constructor Create(); virtual;abstract;
  end;

  TSieError = class
    public
      Message:string
  end;

implementation

end.
