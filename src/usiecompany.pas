unit USieCompany;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, USieClasses;
type
TSieCompany = class(TSieCompanyBase)
private
  _orgTypeNames: specialize  TDictionary<string, string>;
  procedure fillOrgTypeNames();
public

  constructor Create();
end;

implementation

  constructor TSieCompany.Create();
  begin

  end;

  procedure TSieCompany.fillOrgTypeNames();
  begin
    self._orgTypeNames.Add('AB', 'Aktiebolag.');
    _orgTypeNames.Add('E', 'Enskild näringsidkare.');
    _orgTypeNames.Add('HB', 'Handelsbolag.');
    _orgTypeNames.Add('KB', 'Kommanditbolag.');
    _orgTypeNames.Add('EK', 'Ekonomisk förening.');
    _orgTypeNames.Add('KHF', 'Kooperativ hyresrättsförening.');
    _orgTypeNames.Add('BRF', 'Bostadsrättsförening.');
    _orgTypeNames.Add('BF', 'Bostadsförening.');
    _orgTypeNames.Add('SF', 'Sambruksförening.');
    _orgTypeNames.Add('I', 'Ideell förening som bedriver näring.');
    _orgTypeNames.Add('S', 'Stiftelse som bedriver näring.');
    _orgTypeNames.Add('FL', 'Filial till utländskt bolag.');
    _orgTypeNames.Add('BAB', 'Bankaktiebolag.');
    _orgTypeNames.Add('MB', 'Medlemsbank.');
    _orgTypeNames.Add('SB', 'Sparbank.');
    _orgTypeNames.Add('BFL', 'Utländsk banks filial.');
    _orgTypeNames.Add('FAB', 'Försäkringsaktiebolag.');
    _orgTypeNames.Add('OFB', 'Ömsesidigt försäkringsbolag.');
    _orgTypeNames.Add('SE', 'Europabolag.');
    _orgTypeNames.Add('SCE', 'Europakooperativ.');
    _orgTypeNames.Add('TSF', 'Trossamfund.');
    _orgTypeNames.Add('X', 'Annan företagsform.');

  end;

end.

