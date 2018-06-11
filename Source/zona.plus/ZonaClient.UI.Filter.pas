unit ZonaClient.UI.Filter;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation;

type
  TUiFilter = class(TFrame)
    grpSearch: TGroupBox;
    edtSearch: TEdit;
    grpGenre: TGroupBox;
    edtGenre: TEdit;
    grpYear: TGroupBox;
    edtYear: TEdit;
    grpCountry: TGroupBox;
    edtCountry: TEdit;
    grpRait: TGroupBox;
    edtRait: TEdit;
    grpSort: TGroupBox;
    edtSort: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UiFilter: TUiFilter;

implementation

{$R *.fmx}

end.

