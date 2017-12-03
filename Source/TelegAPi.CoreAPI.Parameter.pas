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
  end;

implementation
{ TtgApiParameter }

function TtgApiParameter.IsDefaultValue: Boolean;
begin
  Result := Value.GetReferenceToRawData = DefaultValue.GetReferenceToRawData;
end;

function TtgApiParameter.Skip: Boolean;
begin
  Result := (not Required) and (IsDefaultValue or Value.IsEmpty);
end;

end.

