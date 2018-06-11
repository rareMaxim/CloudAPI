unit ZonaClient.UI.MediaList;

interface

uses
  FrameUiManager,
  ZonaAPI,
  ZonaAPI.Types,
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
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Layouts,
  ZonaAPI.Utils,
  FMX.ListView;

type
{$SCOPEDENUMS ON}
  TTypeItems = (Movie, serial, Search);

  TUiMediaList = class(TFrame)
    lvMediaList: TListView;
    lytPagination: TLayout;
    btnLeft: TCornerButton;
    btnRight: TCornerButton;
    txtCurrentPage: TText;
    img1: TImage;
    tmr1: TTimer;
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure lvMediaListItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
    FTypeView: TTypeItems;
    FCatalog: IznCategory;
    FZona: TZonaAPI;
    FMedia: TMediaLib;
    FFrameStore: TFrameManager;
    FTest: Int64;
    procedure ChangePagination(const IsNext: Boolean);
    procedure FillUI(LItems: IznCategory);
  public
    procedure SetFrameStore(FrameStore: TFrameManager);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
    procedure ShowMovie(const APage: Integer = 1);
    procedure ShowSeries(const APage: Integer = 1);
    procedure Search(const ARequest: string; const APage: Integer = 1);
  end;

implementation

uses
  ZonaClient.UI.ItemView,
  System.Threading;
{$R *.fmx}

procedure TUiMediaList.btnLeftClick(Sender: TObject);
begin
  ChangePagination(False);
end;

procedure TUiMediaList.btnRightClick(Sender: TObject);
begin
  ChangePagination(True);
end;

procedure TUiMediaList.ChangePagination(const IsNext: Boolean);
var
  LChanger: Integer;
begin
  FMedia.Stop;
  if IsNext then
    LChanger := 1
  else
    LChanger := -1;
  if FTypeView = TTypeItems.Movie then
    ShowMovie(FCatalog.pagination.current_page + LChanger)
  else if FTypeView = TTypeItems.Serial then
    ShowSeries(FCatalog.pagination.current_page + LChanger)
  else if FTypeView = TTypeItems.Search then
    Search((FCatalog as IznSearch).query.current, FCatalog.pagination.current_page
      + LChanger);
end;

constructor TUiMediaList.Create(AOwner: TComponent);
begin
  inherited;
  FZona := TZonaAPI.Create(Self);
  FMedia := TMediaLib.Create;
end;

destructor TUiMediaList.Destroy;
begin
  FMedia.Free;
  FreeAndNil(FZona);
  inherited;
end;

procedure TUiMediaList.FillUI(LItems: IznCategory);
var
  LIterator: IznItem;
begin
  lvMediaList.ScrollTo(0);
  lvMediaList.BeginUpdate;
  try
    lvMediaList.Items.Clear;
    FMedia.Clear;
    txtCurrentPage.Text := Format('%d / %d', [LItems.pagination.current_page,
      LItems.pagination.total_pages]);
    for LIterator in LItems.items do
      with lvMediaList.Items.Add do
      begin
        Text := LIterator.name_rus;
      //  Bitmap := img1.Bitmap;
        FMedia.Load(Bitmap, LIterator.cover);
      end;
    FMedia.Run;
  finally
    lvMediaList.EndUpdate;
  end;
end;

procedure TUiMediaList.lvMediaListItemClick(const Sender: TObject; const AItem:
  TListViewItem);
begin
  FFrameStore.getFrame<TUiItemView>.FillInfo(FCatalog.Items[AItem.Index]);
  FFrameStore.Navigate('ItemView');
end;

procedure TUiMediaList.Search(const ARequest: string; const APage: Integer);
begin
  FTypeView := TTypeItems.Search;
  TTask.Run(
    procedure
    begin
      // Async work
      FCatalog := FZona.Search(ARequest, APage);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          // Sync work result
          FillUI(FCatalog);
        end)
    end);
end;

procedure TUiMediaList.SetFrameStore(FrameStore: TFrameManager);
begin
  FFrameStore := FrameStore;
  FFrameStore.RegFrame<TUiItemView>('ItemView');
end;

procedure TUiMediaList.ShowMovie(const APage: Integer);
begin
  FTypeView := TTypeItems.Movie;
  btnLeft.Text := 'ShowMovie';
  TTask.Run(
    procedure
    begin
      FCatalog := FZona.GetMovies(APage);
      TThread.Queue(nil,
        procedure
        begin
          FillUI(FCatalog);
        end)
    end);
end;

procedure TUiMediaList.ShowSeries(const APage: Integer);
begin
  FTypeView := TTypeItems.Serial;
  TTask.Run(
    procedure
    begin
      // Async work
      FCatalog := FZona.GetSeries(APage);
      TThread.Synchronize(nil,
        procedure
        begin
          // Sync work result
          FillUI(FCatalog);
        end)
    end);
end;

procedure TUiMediaList.tmr1Timer(Sender: TObject);
begin
  Inc(FTest);
  btnLeft.Text := FTest.ToString;
end;

end.

