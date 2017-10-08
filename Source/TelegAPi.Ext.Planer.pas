unit TelegAPi.Ext.Planer;

interface

uses
  System.SysUtils;

type
  TTimeAction = class
  private
    FOnAction: TProc;
  published
    property OnAction: TProc read FOnAction write FOnAction;
  end;

implementation

end.

