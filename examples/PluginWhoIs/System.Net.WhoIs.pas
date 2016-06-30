unit System.Net.WhoIs;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Net.Socket;

Type
  TWhoIs = Class
  private
    FServers: TDictionary<string, TPair<String, Word>>;
    FAvaibleDomains: TList<String>;
  public
    Function FullInfo(Const Domain: String): String;
    Function IsAvaible(Const Domain: String): Boolean;
    Constructor Create;

    Procedure RegisterWhoIs(Const Zone: String; Const Server: String;
      Const Port: Word = 43);
    Procedure UnRegisterWhoIs(Const Zone: String);
    destructor Destroy; override;
  published
    property AvaibleDomains: TList<String> read FAvaibleDomains
      write FAvaibleDomains;
    property WhoIsServers: TDictionary < string, TPair < String,
      Word >> read FServers write FServers;
  End;

implementation

{ TWhoIs }

constructor TWhoIs.Create;
begin
  FServers := TDictionary < string, TPair < String, Word >>.Create;
  FAvaibleDomains := TList<string>.Create;
  FAvaibleDomains.AddRange(['No entries found', 'No match for',
    'Nothing found for this query', 'is available for registration']);
  //
  RegisterWhoIs('.com', 'whois.internic.net');
  RegisterWhoIs('.net', 'whois.internic.net');
  RegisterWhoIs('.edu', 'whois.internic.net');
  //
  RegisterWhoIs('.ru', 'whois.r01.ru');
  RegisterWhoIs('.πτ', 'whois.r01.ru');

  RegisterWhoIs('.tv', 'whois.nic.tv');
  RegisterWhoIs('.click', 'whois.uniregistry.net');
  RegisterWhoIs('.xyz', 'whois.nic.xyz');

end;

destructor TWhoIs.Destroy;
begin
  FAvaibleDomains.Free;
  FServers.Free;
  inherited;
end;

procedure TWhoIs.RegisterWhoIs(const Zone, Server: String; const Port: Word);
begin
  FServers.AddOrSetValue(Zone.ToLower,
    TPair<string, Word>.Create(Server, Port));
end;

procedure TWhoIs.UnRegisterWhoIs(const Zone: String);
begin
  FServers.Remove(Zone.ToLower);
end;

function TWhoIs.FullInfo(const Domain: String): String;
var
  LSocket: TSocket;
  LServPort: TPair<String, Word>;
begin
  LSocket := TSocket.Create(TSocketType.TCP);
  try
    if NOT FServers.TryGetValue(ExtractFileExt(Domain.ToLower), LServPort) then
      Exit('no find whoIs server');
    LSocket.Connect(LServPort.Key, '', '', LServPort.Value);
    LSocket.Send(Domain + #13#10);
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

function TWhoIs.IsAvaible(const Domain: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FAvaibleDomains.Count - 1 do
    if FullInfo(Domain).Trim.ToLower.Contains(FAvaibleDomains[I].ToLower) then
      Exit(True);
end;

end.
