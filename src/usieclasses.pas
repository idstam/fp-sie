unit USieClasses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TSieObject = class;

  TSieAccount = class
  private
  public
    Number: string;
    Name: string;
    AccUnit: string;
    AccType: string;
    SRU: specialize THashSet<string>;
  end;

  TSieCompany = class
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

  TSieDimension = class
  private
    _parent: TSieDimension;
  public
    Number: string;
    Name: string;
    IsDefault: boolean;
    SubDim: specialize THashSet<TSieDimension>;
    Objects: specialize TDictionary<string, TSieObject>;

  end;

  TSieObject = class
  private
  public
    Dimension: TSieDimension;
    Number: string;
    Name: string;
  end;


implementation

end.
