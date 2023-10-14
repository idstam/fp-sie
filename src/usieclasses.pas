unit USieClasses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Nullable;

type
  TSieObject = class;
  TSieDimension = class;
  TSieVoucherRow = class;
  TSieAccount = class;
  TSieBookingYear = class;
  TSiePeriodValue = class;
  TSieVoucher = class;
  TSieError = class;

  THashSetString = specialize THashSet<string>;
  THashSetSieDimension = specialize THashSet<TSieDimension>;

  TNullableCurrency = specialize TNullable<currency>;
  TNullableInteger = specialize TNullable<integer>;

  TListSieObject = specialize TList<TSieObject>;
  TListSieVoucherRow = specialize TList<TSieVoucherRow>;

  TDictStringSieObject = specialize TDictionary<string, TSieObject>;
  TDictStringString = specialize TDictionary<string, string>;
  TDictStringSieAccount = specialize TDictionary<string, TSieAccount>;

  TDictStringSieDimension = specialize TDictionary<string, TSieDimension>;
  TDictStringSieBookingYear = specialize TDictionary<string, TSieBookingYear>;
  TListSiePeriodValue = specialize TList<TSiePeriodValue>;
  TListSieVoucher = specialize TList<TSieVoucher>;
  TListSieError = specialize TList<TSieError>;

  TSieAccount = class
  private
  public
    Number: string;
    Name: string;
    AccUnit: string;
    AccType: string;
    SRU: THashSetString;
    constructor Create(aNumber: string);
  end;

  TSieCompany = class
  private
    orgTypeNames: TDictStringString;
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
    constructor Create();
  end;

  TSieDimension = class
  private
    _parent: TSieDimension;
  public
    Number: string;
    Name: string;
    IsDefault: boolean;
    SubDim: THashSetSieDimension;
    Objects: TDictStringSieObject;
    constructor Create(aNumber: string; aName: string; aIsDefault: boolean);
    constructor Create(aNumber: string; aName: string;
      aParent: TSieDimension; aIsDefault: boolean);
  end;

  TSieObject = class
  private
  public
    Dimension: TSieDimension;
    Number: string;
    Name: string;
  end;

  TSiePeriodValue = class
  private
  public
    Account: TSieAccount;
    YearNr: integer;
    Period: integer;
    Amount: currency;
    Quantity: TNullableCurrency;
    Objects: TListSieObject;
    Token: string;
    constructor Create();
  end;

  TSieBookingYear = class
    ID: integer;
    StartDate: string;
    EndDate: string;
  end;

  TSieVoucherRow = class
  private
  public
    Account: TSieAccount;
    Objects: TListSieObject;
    Amount: currency;
    RowDate: integer;
    Text: string;
    Quantity: TNullableCurrency;
    CreatedBy: string;
    Token: string;
  end;

  TSieVoucher = class
  private
  public
    Series: string;
    Number: string;
    VoucherDate: string;
    Text: string;
    CreatedDate: string;
    CreatedBy: string;
    Token: string;
    Rows: TListSieVoucherRow;
    constructor Create();
  end;

  TSieDocument = class
  private
  public
    FileName: string;
    IgnoreRTRANS: boolean;
    IgnoreBTRANS: boolean;
    IgnoreMissingDate: boolean;
    IgnoreMissingOMFATTNING: boolean;
    DateFormat: string;
    DIM: TDictStringSieDimension;
    FLAGGA: integer;
    FNAMN: TSieCompany;
    FORMAT: string;
    GEN_DATE: string;
    GEN_NAMN: string;
    IB: TListSiePeriodValue;
    KONTO: TDictStringSieAccount;
    KSUMMA: longint;
    KPTYP: string;
    OMFATTN: string;
    OBJEKT: TDictStringSieObject;
    OIB: TListSiePeriodValue;
    OUB: TListSiePeriodValue;
    PBUDGET: TListSiePeriodValue;
    //#PROGRAM
    PROGRAMS: TStringList;
    PROSA: string;
    PSALDO: TListSiePeriodValue;
    RAR: TDictStringSieBookingYear;
    RES: TListSiePeriodValue;
    SIETYP: integer;
    TAXAR: integer;
    UB: TListSiePeriodValue;
    VALUTA: string;
    ValidationErrors: TListSieError;
    VER: TListSieVoucher;
    //CRC
    constructor Create();
  published

  end;

  TErrorTypes = (SieInvalidChecksumError, SieInvalidFileError,
    SieDateError, SieInvalidFeatureError,
    SieMissingMandatoryDateError, SieMissingObjectError, SieVoucherMissmatchError,
    SieNotImplementedError,
    SieMissingFieldError);

  TSieError = class
  public
    Error: TErrorTypes;
    Message: string;
    constructor Create(aError: TErrorTypes; aMessage: string);
  end;

implementation

constructor TSiePeriodValue.Create();
begin
  Objects := TListSieObject.Create();
end;

constructor TSieAccount.Create(aNumber: string);
begin
  Number := aNumber;
  SRU := THashSetString.Create;
end;

constructor TSieCompany.Create();
begin
  self.orgTypeNames := TDictStringString.Create();
  orgTypeNames.Add('AB', 'Aktiebolag.');
  orgTypeNames.Add('E', 'Enskild näringsidkare.');
  orgTypeNames.Add('HB', 'Handelsbolag.');
  orgTypeNames.Add('KB', 'Kommanditbolag.');
  orgTypeNames.Add('EK', 'Ekonomisk förening.');
  orgTypeNames.Add('KHF', 'Kooperativ hyresrättsförening.');
  orgTypeNames.Add('BRF', 'Bostadsrättsförening.');
  orgTypeNames.Add('BF', 'Bostadsförening.');
  orgTypeNames.Add('SF', 'Sambruksförening.');
  orgTypeNames.Add('I', 'Ideell förening som bedriver näring.');
  orgTypeNames.Add('S', 'Stiftelse som bedriver näring.');
  orgTypeNames.Add('FL', 'Filial till utländskt bolag.');
  orgTypeNames.Add('BAB', 'Bankaktiebolag.');
  orgTypeNames.Add('MB', 'Medlemsbank.');
  orgTypeNames.Add('SB', 'Sparbank.');
  orgTypeNames.Add('BFL', 'Utländsk banks filial.');
  orgTypeNames.Add('FAB', 'Försäkringsaktiebolag.');
  orgTypeNames.Add('OFB', 'Ömsesidigt försäkringsbolag.');
  orgTypeNames.Add('SE', 'Europabolag.');
  orgTypeNames.Add('SCE', 'Europakooperativ.');
  orgTypeNames.Add('TSF', 'Trossamfund.');
  orgTypeNames.Add('X', 'Annan företagsform.');
end;

constructor TSieVoucher.Create();
begin
  Rows := TListSieVoucherRow.Create();
end;

constructor TSieError.Create(aError: TErrorTypes; aMessage: string);
begin
  Error := aError;
  Message := aMessage;
end;

constructor TSieDimension.Create(aNumber: string; aName: string; aIsDefault: boolean);
begin
  Number := aNumber;
  Name := aName;
  IsDefault := aIsDefault;
end;

constructor TSieDimension.Create(aNumber: string; aName: string;
  aParent: TSieDimension; aIsDefault: boolean);
begin
  Number := aNumber;
  Name := aName;
  IsDefault := aIsDefault;
end;

constructor TSieDocument.Create();
begin
  self.DateFormat := 'yyyyMMdd';

end;

end.
