unit RadioRecord;

interface

uses
  CloudAPI.BaseComponent,
  RadioRecord.Types;

type
  TRadioRecord = class(TCloudApiBaseComponent)
  protected
    procedure DoInitApiCore; override;
  public
    function GetStations: TArray<IrrStation>;
  end;

implementation

uses
  CloudAPI.Utils.Json,
  System.SysUtils,
  System.JSON;
{ TRadioRecord }

procedure TRadioRecord.DoInitApiCore;
begin
  inherited;
  Domain := 'http://www.radiorecord.ru/radioapi/';
  GetRequest.OnDataReceiveAsString :=
    function(AInput: string): string
    var
      LJSON: TJSONObject;
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AInput);
      Result := '';
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        Result := LJSON.GetValue('result').ToString;
      finally
        LJSON.Free;
      end;
    end;
  GetRequest.OnDataSend :=
    procedure(AUrl, AData, AHeaders: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AUrl, AData);
    end;
end;

function TRadioRecord.GetStations: TArray<IrrStation>;
begin
  Result := TBaseJson.AsArray<IrrStation>(TrrStation, GetRequest.SetMethod('stations') //
    .ExecuteAsString);
end;

end.

