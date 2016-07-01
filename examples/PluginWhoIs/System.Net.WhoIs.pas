unit System.Net.WhoIs;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Net.Socket;

Type
  TOnCheckDomain = procedure(Const URL: String; Const IsFree: Boolean);

  TWhoIs = Class
  Private Const
    WHOIS_PORT = 43;
  private
    FServers: TDictionary<string, TPair<String, String>>;
  public
    Function FullInfo(Const URL: String): String;
    Function IsAvaible(Const URL: String): Boolean;
    Procedure FindFreeZone(Const Name: String; Zones: TArray<String>;
      OnCheck: TOnCheckDomain);
    Constructor Create;

    Procedure RegisterWhoIs(Const Zone: String; Const Server: String;
      Const IsFreeSign: String);
    Procedure UnRegisterWhoIs(Const Zone: String);
    destructor Destroy; override;
  published
    property WhoIsServers: TDictionary < string, TPair < String,
      String >> read FServers write FServers;
  End;

implementation

{ TWhoIs }

constructor TWhoIs.Create;
begin
  FServers := TDictionary < string, TPair < String, String >>.Create;
  //
  RegisterWhoIs('.com', 'whois.internic.net', 'No match for ');
  RegisterWhoIs('.net', 'whois.internic.net', 'No match for ');
  RegisterWhoIs('.edu', 'whois.internic.net', 'No match for ');
  //
  RegisterWhoIs('.ru', 'whois.r01.ru', '');
  RegisterWhoIs('.πτ', 'whois.r01.ru', '');

  RegisterWhoIs('.tv', 'whois.nic.tv', '');
  RegisterWhoIs('.click', 'whois.uniregistry.net',
    'available for registration');
  RegisterWhoIs('.xyz', 'whois.nic.xyz', '');
end;

destructor TWhoIs.Destroy;
begin
  FServers.Free;
  inherited;
end;

procedure TWhoIs.UnRegisterWhoIs(const Zone: String);
begin
  FServers.Remove(Zone.ToLower);
end;

procedure TWhoIs.FindFreeZone(const Name: String; Zones: TArray<String>;
  OnCheck: TOnCheckDomain);
var
  i: Integer;
begin
  for i := Low(Zones) to High(Zones) do
    OnCheck(Name + '.' + Zones[i], IsAvaible(Name + '.' + Zones[i]));
end;

function TWhoIs.FullInfo(const URL: String): String;
var
  LSocket: TSocket;
  LServSign: TPair<String, String>;
begin
  LSocket := TSocket.Create(TSocketType.TCP);
  try
    if NOT FServers.TryGetValue(ExtractFileExt(URL.ToLower), LServSign) then
      raise Exception.Create('no find whoIs server');
    LSocket.Connect(LServSign.Key, '', '', WHOIS_PORT);
    LSocket.Send(URL + #13#10);
    while True do
    Begin
      Result := LSocket.ReceiveString;
      if Result <> '' then
        break;
    End;
  finally
    LSocket.Free;
  end;
end;

function TWhoIs.IsAvaible(const URL: String): Boolean;
var
  i: Integer;
  LServSign: TPair<String, String>;
begin
  if NOT FServers.TryGetValue(ExtractFileExt(URL.ToLower), LServSign) then
    raise Exception.Create('no find whoIs server');

  if FullInfo(URL).Trim.ToLower.Contains(LServSign.Value) then
    Exit(True);
end;

procedure TWhoIs.RegisterWhoIs(const Zone, Server, IsFreeSign: String);
begin
  FServers.AddOrSetValue(Zone.ToLower, TPair<string, string>.Create(Server,
    IsFreeSign));
end;

end.
