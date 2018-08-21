unit InvisionCommunity.Base;

interface

uses
  CloudAPI.BaseComponent;

type
  TInvCommBase = class(TCloudApiBaseComponent)
  private
    FToken: string;
    FUrl: string;
    procedure SetUrl(const Value: string);
    procedure SetToken(const Value: string);
  protected
    procedure DoInitApiCore; override;
  public
    property Token: string read FToken write SetToken;
    property Url: string read FUrl write SetUrl;
  end;

implementation

uses
  System.SysUtils,
  CloudAPI.Exception,
  CloudAPI.Request,
  System.NetEncoding,
  System.Json;
{ TInvCommBase }

procedure TInvCommBase.DoInitApiCore;
begin
  inherited;
  GetRequest.StoreAutoFormat := TStoreFormat.InFormData;
  GetRequest.OnDataReceiveAsString := function(AInput: string): string
    var
      LJSON: TJSONObject;
      LTest: string;
      LExcept: ECloudApiException;
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AInput);
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        if LJSON.TryGetValue<string>('errorCode', LTest) then
        begin
          LExcept := ECloudApiException.Create(LJSON.Values['errorCode'].Value, LJSON.Values['errorMessage'].Value);
          try
            DoCallLogEvent(LExcept, False);
            Exit;
          finally
            LExcept.Free;
          end;
        end;
         Result := AInput;
      finally
        LJSON.Free;
      end
    end;
  GetRequest.OnStaticFill := procedure
    begin
      GetRequest.Domain := Domain;
      GetRequest.AddParameter('Authorization', 'Basic ' + TNetEncoding.Base64.Encode(FToken + ':'), '', True,
        TStoreFormat.InHeader);
    end;
  GetRequest.OnDataSend := procedure(AUrl, AData, AHeaders: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AUrl, AData);
    end;
end;

procedure TInvCommBase.SetToken(const Value: string);
begin
  FToken := Value;
end;

procedure TInvCommBase.SetUrl(const Value: string);
begin
  FUrl := Value;
  GetRequest.Domain := Value;
end;

end.
