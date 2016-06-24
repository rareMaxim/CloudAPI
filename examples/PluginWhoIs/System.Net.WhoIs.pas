unit System.Net.WhoIs;

interface

uses
  System.SysUtils,
  System.Net.Socket;

Type
  TWhoIs = Class
    Function WhoIs(Const Name: String): String;
  End;

implementation

uses
  idWhois;
{ TWhoIs }

function TWhoIs.WhoIs(const Name: String): String;
var
  LSocket: TSocket;
begin
  LSocket := TSocket.Create(TSocketType.TCP);
  try
    LSocket.Connect('whois.internic.net', '', '', 43);
    LSocket.Send(Name + #13#10);
    while true do
    Begin
      Result := LSocket.ReceiveString;
      if Result <> '' then
        break;
    End;
  
  finally
    LSocket.Free;
  end;
end;

end.
