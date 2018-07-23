unit ZonaRu.Query;

interface

type
  TZonaQuery = class
  private
    Fabuse: string;
    Fadult: boolean;
    Ftor_count: string;
    Findexed: string;
    Fserial: boolean;
    FNot_genreId: string;
    FID: Int64;
  public
    function ToCatalog: string;
    function ToItem: string;
    constructor Create;
  published

    property ID: Int64 read FID write FID;
    property abuse: string read Fabuse write Fabuse;
    property adult: boolean read Fadult write Fadult;
    property tor_count: string read Ftor_count write Ftor_count;
    property indexed: string read Findexed write Findexed;
    property serial: boolean read Fserial write Fserial;
    property Not_genreId: string read FNot_genreId write FNot_genreId;
  end;

implementation

uses
  System.SysUtils;
{ TZonaQuery }

constructor TZonaQuery.Create;
begin
  abuse := 'zona';
  adult := False;
  tor_count := '[1+TO+2147483647]';
  indexed := '[1+TO+7]';
  serial := False;
  Not_genreId := '(12+OR+15+OR+25+OR+26+OR+1747+OR+28+OR+27+OR+tv)';
  FID := 0;
end;

function TZonaQuery.ToCatalog: string;
begin
  Result := '(NOT(abuse:%abuse%)AND(adult:%adult%)AND(tor_count:%tor_count%)AND(indexed:%indexed%)AND(serial:%serial%)NOT(genreId:%genreId%))';
  Result := Result //
    .Replace('%abuse%', abuse) //
    .Replace('%adult%', adult.ToString(TUseBoolStrs.True).ToLower) //
    .Replace('%tor_count%', tor_count) //
    .Replace('%indexed%', indexed) //
    .Replace('%serial%', serial.ToString(TUseBoolStrs.True).ToLower) //
    .Replace('%genreId%', Not_genreId) //
    ;
end;

function TZonaQuery.ToItem: string;
begin
  Result := '((id:%ID%))'.Replace('%ID%', ID.ToString);
end;

end.
