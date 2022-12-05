unit SearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TTSearchForm }

  TTSearchForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    FText: TStrings;
  public

  end;

  procedure ShowSearchForm(Owner: TComponent; aText: TStrings);
var
  TSearchForm: TTSearchForm;
  FormCount: Integer;

implementation

{$R *.lfm}

procedure ShowSearchForm(Owner: TComponent;  aText: TStrings);
var
  SearchForm: TTSearchForm;
begin
  SearchForm := TTSearchForm.Create(Owner);
  try
    SearchForm.Name := 'MyForm' + IntToStr(FormCount);
    SearchForm.Show;
  except
    SearchForm.Free;
  end;
end;

{ TTSearchForm }

procedure TTSearchForm.Button1Click(Sender: TObject);
begin

end;

end.

