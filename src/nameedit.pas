unit NameEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type
  TfrmNameEdit = class(TForm)
    btnCancel : TBitBtn;
    btnOK : TBitBtn;
    btnOptions : TBitBtn;
    editPrefix : TEdit;
    editName : TEdit;
    lblClass : TLabel;
    btnPrefixOK : TSpeedButton;
    btnPrefixCancel : TSpeedButton;
    btnPrefixEdit : TSpeedButton;
    procedure btnPrefixOKClick(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TfrmNameEdit }

procedure TfrmNameEdit.btnPrefixOKClick(Sender : TObject);
begin

end;

end.

