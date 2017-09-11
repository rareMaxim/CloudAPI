unit TelegAPI.Utils.KbDesigner;

interface

uses
  FMX.Layouts, TelegaPi.Types.ReplyMarkups;

type
  TtgKeyBoarDesigner = class(TGridLayout)
  private
    FRowCount: Integer;
    FColCount: Integer;
  public
    function AsIReplyMarkup: IReplyMarkup;
  published
    property RowCount: Integer read FRowCount write FRowCount;
    property ColCount: Integer read FColCount write FColCount;
  end;

implementation

{ TtgKeyBoarDesigner }

function TtgKeyBoarDesigner.AsIReplyMarkup: IReplyMarkup;
begin

end;

end.

