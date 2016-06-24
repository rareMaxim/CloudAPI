unit System.Net.WhoIs;

interface

uses
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
    while LSocket.ReceiveLength = 0 do
      Result := LSocket.ReceiveString;
  finally
    LSocket.Free;
  end;
end;

end.
