unit USieAccount;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TSieAccount = class
private
public
  Number:string;
  Name: string;
  AccUnit: string;
  AccType: string;
  SRU: specialize THashSet<string>;
end;

implementation

end.

