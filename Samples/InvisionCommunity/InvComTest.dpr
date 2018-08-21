program InvComTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  InvisionCommunity.System in '..\..\Source\invisioncommunity.com\InvisionCommunity.System.pas',
  InvisionCommunity.System.Types in '..\..\Source\invisioncommunity.com\InvisionCommunity.System.Types.pas',
  InvisionCommunity.Forums in '..\..\Source\invisioncommunity.com\InvisionCommunity.Forums.pas',
  InvisionCommunity.Forums.Types in '..\..\Source\invisioncommunity.com\InvisionCommunity.Forums.Types.pas',
  InvisionCommunity.Types in '..\..\Source\invisioncommunity.com\InvisionCommunity.Types.pas',
  InvisionCommunity in '..\..\Source\invisioncommunity.com\InvisionCommunity.pas',
  InvisionCommunity.Base in '..\..\Source\invisioncommunity.com\InvisionCommunity.Base.pas';

procedure Test;
var
  ic: TInvComm;
  LTopics: IicArray<IicTopicObject>;
  I: Integer;
begin
  ic := TInvComm.Create;
  ic.Url := 'http://fire-monkey.ru';
  ic.Token := '18801d4912ac06457bfb3b9fa0123f7a';
  try
    with ic.System.Hello do
    begin
      Writeln(communityName);
      Writeln(communityUrl);
      Writeln(ipsVersion);
    end;
    LTopics := ic.Forums.GetTopics('', '', -1, -1, -1, -1, -1, -1, -1, '', '', -1);
    Writeln(ic.Forums.GetTopic(9999999  ).Url);
    // LTopics := ic.Forums.GetTopics('', '', -1, -1, -1, -1, -1, -1, -1, '', '', LTopics.TotalPages);

    begin
      for I := Low(LTopics.Results) to High(LTopics.Results) do
        Writeln(LTopics.Results[I].Title);
    end;
    Writeln(ic.Forums.GetTopic(LTopics.Results[0].ID).Url);
  finally
    ic.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Test;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
