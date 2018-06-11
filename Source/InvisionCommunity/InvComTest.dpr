program InvComTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  InvisionCommunity.System in 'InvisionCommunity.System.pas',
  InvisionCommunity.System.Types in 'InvisionCommunity.System.Types.pas',
  InvisionCommunity.Core.JsonBaseClass in 'InvisionCommunity.Core.JsonBaseClass.pas',
  InvisionCommunity.Core.Api in 'InvisionCommunity.Core.Api.pas',
  InvisionCommunity.Exceptions in 'InvisionCommunity.Exceptions.pas',
  InvisionCommunity.Forums in 'InvisionCommunity.Forums.pas',
  InvisionCommunity.Forums.Types in 'InvisionCommunity.Forums.Types.pas',
  InvisionCommunity.Types in 'InvisionCommunity.Types.pas';

procedure Test;
var
  icSystem: IicSystem;
  icForum: IicForums;
  LTopics: IicArray<IicTopicObject>;
  I: Integer;
begin
  icSystem := TicSystem.Create('http://fire-monkey.ru', '');
  icForum := TicForums.Create('http://fire-monkey.ru', '');
  try
    with icSystem.Hello do
    begin
      Writeln(communityName);
      Writeln(communityUrl);
      Writeln(ipsVersion);
    end;
    LTopics := icForum.GetTopics('', '', -1, -1, -1, -1, -1, -1, -1, '', '', -1);
    LTopics := icForum.GetTopics('', '', -1, -1, -1, -1, -1, -1, -1, '', '', LTopics.TotalPages);

    begin
      for I := Low(LTopics.Results) to High(LTopics.Results) do
        Writeln(LTopics.Results[I].Title);
    end;

  finally
    // icSystem.Free;
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

