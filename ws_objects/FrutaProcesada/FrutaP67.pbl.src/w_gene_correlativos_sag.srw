$PBExportHeader$w_gene_correlativos_sag.srw
$PBExportComments$Ventana de Consulta ultima inspeccion
forward
global type w_gene_correlativos_sag from w_para_informes
end type
type sle_mensaje from singlelineedit within w_gene_correlativos_sag
end type
type st_2 from statictext within w_gene_correlativos_sag
end type
type st_44 from statictext within w_gene_correlativos_sag
end type
type cbx_1 from checkbox within w_gene_correlativos_sag
end type
type cbx_2 from checkbox within w_gene_correlativos_sag
end type
type cbx_3 from checkbox within w_gene_correlativos_sag
end type
type cbx_4 from checkbox within w_gene_correlativos_sag
end type
type cbx_5 from checkbox within w_gene_correlativos_sag
end type
end forward

global type w_gene_correlativos_sag from w_para_informes
integer x = 14
integer y = 32
integer width = 2441
integer height = 1284
string title = "Resetea Correlativos SAG"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
event ue_validapassword ( )
sle_mensaje sle_mensaje
st_2 st_2
st_44 st_44
cbx_1 cbx_1
cbx_2 cbx_2
cbx_3 cbx_3
cbx_4 cbx_4
cbx_5 cbx_5
end type
global w_gene_correlativos_sag w_gene_correlativos_sag

type variables
str_busqueda	istr_busq
str_mant 		istr_mant
end variables

event ue_validapassword();str_mant			lstr_mant	

lstr_mant.Argumento[1]	=	"Producción"
lstr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

on w_gene_correlativos_sag.create
int iCurrent
call super::create
this.sle_mensaje=create sle_mensaje
this.st_2=create st_2
this.st_44=create st_44
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.cbx_4=create cbx_4
this.cbx_5=create cbx_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_mensaje
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_44
this.Control[iCurrent+4]=this.cbx_1
this.Control[iCurrent+5]=this.cbx_2
this.Control[iCurrent+6]=this.cbx_3
this.Control[iCurrent+7]=this.cbx_4
this.Control[iCurrent+8]=this.cbx_5
end on

on w_gene_correlativos_sag.destroy
call super::destroy
destroy(this.sle_mensaje)
destroy(this.st_2)
destroy(this.st_44)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.cbx_4)
destroy(this.cbx_5)
end on

event open;call super::open; PostEvent("ue_validapassword")

end event

type pb_excel from w_para_informes`pb_excel within w_gene_correlativos_sag
end type

type st_computador from w_para_informes`st_computador within w_gene_correlativos_sag
end type

type st_usuario from w_para_informes`st_usuario within w_gene_correlativos_sag
integer x = 2071
end type

type st_temporada from w_para_informes`st_temporada within w_gene_correlativos_sag
end type

type p_logo from w_para_informes`p_logo within w_gene_correlativos_sag
end type

type st_titulo from w_para_informes`st_titulo within w_gene_correlativos_sag
integer y = 300
integer width = 1664
string text = "Resetea Correlativos SAG"
end type

type pb_acepta from w_para_informes`pb_acepta within w_gene_correlativos_sag
string tag = "Imprimir Reporte"
integer x = 2062
integer y = 496
integer taborder = 20
fontcharset fontcharset = ansi!
boolean underline = true
boolean default = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Integer	li_filtro1, li_filtro2, li_filtro3, li_filtro4, li_filtro5
Long	ll_Busca, ll_Fila

sle_Mensaje.Text	= 'Proceso en Curso...'

IF cbx_1.Checked THEN
	li_filtro1 = 1
ELSE
	li_filtro1 = 0
END IF	

IF cbx_2.Checked THEN
	li_filtro2 = 1
ELSE
	li_filtro2 = 0
END IF	

IF cbx_3.Checked THEN
	li_filtro3 = 1
ELSE
	li_filtro3 = 0
END IF	

IF cbx_4.Checked THEN
	li_filtro4 = 1
ELSE
	li_filtro4 = 0
END IF	

IF cbx_5.Checked THEN
	li_filtro5 = 1
ELSE
	li_filtro5 = 0
END IF	

Declare correlativos_sag Procedure For dbo.FProc_correlativos_sag
	@filtro1 	= :li_filtro1,
	@filtro2		= :li_filtro2,
	@filtro3		= :li_filtro3,
	@filtro4		= :li_filtro4,
	@filtro5		= :li_filtro5;
Execute correlativos_sag ;
	
IF sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Procedimiento FProc_correlativos_sag ")
END IF

Close correlativos_sag;

Commit;

sle_Mensaje.Text	= 'Proceso Terminado...'


SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_gene_correlativos_sag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2057
integer y = 776
integer taborder = 30
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type sle_mensaje from singlelineedit within w_gene_correlativos_sag
integer x = 265
integer y = 904
integer width = 1637
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_gene_correlativos_sag
integer x = 279
integer y = 804
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mensaje"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_44 from statictext within w_gene_correlativos_sag
integer x = 251
integer y = 432
integer width = 1664
integer height = 632
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_gene_correlativos_sag
integer x = 558
integer y = 680
integer width = 763
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "SAG CAMBIO DESTINO"
boolean lefttext = true
end type

type cbx_2 from checkbox within w_gene_correlativos_sag
integer x = 558
integer y = 464
integer width = 763
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "SAG CONDICION"
boolean lefttext = true
end type

type cbx_3 from checkbox within w_gene_correlativos_sag
integer x = 558
integer y = 752
integer width = 763
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "SAG DESPACHOS"
boolean lefttext = true
end type

type cbx_4 from checkbox within w_gene_correlativos_sag
integer x = 558
integer y = 536
integer width = 763
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "SAG INSPECCION"
boolean lefttext = true
end type

type cbx_5 from checkbox within w_gene_correlativos_sag
integer x = 558
integer y = 608
integer width = 763
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "SAG REPALLETIZADOS"
boolean lefttext = true
end type

