unit TelegAPI.ApiRequest;

interface

uses
  TelegAPI.Types,
  CloudAPI.Request;

type
  ItgApiRequest = interface(IApiRequest)
    ['{0C6EFEE6-67B5-426F-92B0-24925838A618}']
    function AddParameter(const AKey: string; AValue, ADefaultValue:
      TFileToSend; const ARequired: Boolean; const AStoreFormat: TStoreFormat): ItgApiRequest; overload;
  end;

  TtgApiRequest = class(TApiRequest, ItgApiRequest)
  public
    function AddParameter(const AKey: string; AValue, ADefaultValue:
      TFileToSend; const ARequired: Boolean; const AStoreFormat: TStoreFormat): ItgApiRequest; overload;
  end;

implementation

uses
  CloudAPI.Utils.Json,
  System.SysUtils;
{ TtgApiRequest }

function TtgApiRequest.AddParameter(const AKey: string; AValue, ADefaultValue:
  TFileToSend; const ARequired: Boolean; const AStoreFormat: TStoreFormat): ItgApiRequest;
begin
  if ARequired and (AValue.Equals(ADefaultValue) or AValue.IsEmpty) then
  begin
    DoHaveException(Exception.Create('Not assigned required data'));
    Exit;
  end;
  Result := Self;
  case AValue.Tag of
    TtgFileToSendTag.FromStream:
      StoreMultipartForm.AddStream(AKey, AValue.Content, AValue.Data);
    TtgFileToSendTag.FromFile:
      StoreMultipartForm.AddFile(AKey, AValue.Data);
    TtgFileToSendTag.ID, TtgFileToSendTag.FromURL:
      AddParameter(AKey, AValue.Data, '', ARequired, AStoreFormat);
  else
    DoHaveException(Exception.Create('Cant convert TTgFileToSend: Unknown prototype tag'));
    Exit;
  end;
  if Assigned(AValue) then
    FreeAndNil(AValue);
  if Assigned(ADefaultValue) then
    FreeAndNil(ADefaultValue);
end;

end.

