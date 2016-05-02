unit TelegAPi.Game.Statistic;

interface

uses
  TelegAPi.Types;

Type
  TTelegaGameStatisticItem = Class
  private
    FUser: TTelegaUser;
    FGameName: String;
    FCount: Integer;
    FDate: TDateTime;
  public
    constructor Create(Const Game: String; User: TTelegaUser; Count: Integer;
      Date: TDateTime = 0.0);
  published
    property User: TTelegaUser read FUser write FUser;
    property GameName: String read FGameName write FGameName;
    property Count: Integer read FCount write FCount;
    property Date: TDateTime read FDate write FDate;
  End;

  TTelegaGameStatistic = Class

  End;

implementation

uses
  System.SysUtils;

{ TTelegaGameStatisticItem }

constructor TTelegaGameStatisticItem.Create(const Game: String; User: TTelegaUser; Count: Integer;
  Date: TDateTime);
begin
  Self.FGameName := Game;
  Self.FUser := User;
  Self.FCount := Count;
  { TODO -oOwner -cGeneral : ѕроверить, будет ли выполн€тьс€ равенство }
  if Date = 0.0 then
    Date := Now;
end;

end.
