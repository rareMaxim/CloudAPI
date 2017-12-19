unit TelegAPi.CoreAPI.Parameter;

interface

uses
  System.Rtti;

type
  TtgApiParameter = class
    Key: string;
    Value: TValue;
    DefaultValue: TValue;
    Required: Boolean;
    function IsDefaultValue: Boolean;
    function Skip: Boolean;
    constructor Create(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean = False);
  end;

implementation

uses
  System.SysUtils;
{ TtgApiParameter }

constructor TtgApiParameter.Create(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean);
begin
  Key := AKey;
  Value := AValue;
  DefaultValue := ADefaultValue;
  Required := ARequired;
end;

function TtgApiParameter.IsDefaultValue: Boolean;
begin
  try
    Result := Value.AsVariant = DefaultValue.AsVariant; // <-- DANGER
  except
    raise EProgrammerNotFound.Create('Несовместимые типы параметров');
  end;
end;

function TtgApiParameter.Skip: Boolean;
begin
  Result := (not Required) and (IsDefaultValue or Value.IsEmpty);
end;

end.

