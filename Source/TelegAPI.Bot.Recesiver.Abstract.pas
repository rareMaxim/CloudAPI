unit TelegAPI.Bot.Recesiver.abstract;

interface

uses
  TelegAPI.Base,
  TelegAPI.Bot,
  TelegAPI.Types,
  System.Classes;

type
  TtgAbstractRecesiver = class(TtgAbstractComponent)
  private
    FMessageOffset: Integer;
    FBot: TTelegramBot;
    FPollingInterval: Integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    {$REGION 'Property|Свойства'}
    property Bot: TTelegramBot read FBot write FBot;
    /// <summary>
    ///   The current message offset
    /// </summary>
    property MessageOffset: Integer read FMessageOffset write FMessageOffset default 0;
    /// <summary>
    ///   Задержка между опросами
    /// </summary>
    property PollingInterval: Integer read FPollingInterval write FPollingInterval default 1000;
    {$ENDREGION}
  end;

  TtgRecesiverAbstractCore = class(TThread)
  private
    FParent: TtgAbstractRecesiver;
  protected
  public
    property Parent: TtgAbstractRecesiver read FParent write FParent;
  end;

implementation

uses
  System.SysUtils;

{ TtgAbstractRecesiver }

constructor TtgAbstractRecesiver.Create(AOwner: TComponent);
begin
  inherited;
  FPollingInterval := 1000;
  FMessageOffset := 0;
end;

end.

