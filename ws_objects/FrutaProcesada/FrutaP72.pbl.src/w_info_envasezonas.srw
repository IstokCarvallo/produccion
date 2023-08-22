$PBExportHeader$w_info_envasezonas.srw
forward
global type w_info_envasezonas from w_para_informes
end type
type st_4 from statictext within w_info_envasezonas
end type
type dw_tipoprodu from datawindow within w_info_envasezonas
end type
type cbx_productor from checkbox within w_info_envasezonas
end type
type st_1 from statictext within w_info_envasezonas
end type
type st_5 from statictext within w_info_envasezonas
end type
type st_2 from statictext within w_info_envasezonas
end type
type st_3 from statictext within w_info_envasezonas
end type
type em_fech_ini from editmask within w_info_envasezonas
end type
type em_fech_fin from editmask within w_info_envasezonas
end type
type dw_2 from datawindow within w_info_envasezonas
end type
type st_6 from statictext within w_info_envasezonas
end type
end forward

global type w_info_envasezonas from w_para_informes
integer x = 14
integer y = 32
integer width = 2642
integer height = 1796
string title = "Envases por Zona"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
st_4 st_4
dw_tipoprodu dw_tipoprodu
cbx_productor cbx_productor
st_1 st_1
st_5 st_5
st_2 st_2
st_3 st_3
em_fech_ini em_fech_ini
em_fech_fin em_fech_fin
dw_2 dw_2
st_6 st_6
end type
global w_info_envasezonas w_info_envasezonas

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	dw_tipopro, dw_cliente
end variables

on w_info_envasezonas.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_tipoprodu=create dw_tipoprodu
this.cbx_productor=create cbx_productor
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.st_3=create st_3
this.em_fech_ini=create em_fech_ini
this.em_fech_fin=create em_fech_fin
this.dw_2=create dw_2
this.st_6=create st_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_tipoprodu
this.Control[iCurrent+3]=this.cbx_productor
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.em_fech_ini
this.Control[iCurrent+9]=this.em_fech_fin
this.Control[iCurrent+10]=this.dw_2
this.Control[iCurrent+11]=this.st_6
end on

on w_info_envasezonas.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_tipoprodu)
destroy(this.cbx_productor)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_fech_ini)
destroy(this.em_fech_fin)
destroy(this.dw_2)
destroy(this.st_6)
end on

event open;call super::open;dw_2.GetChild("clie_codigo", dw_cliente)
dw_cliente.SetTransObject(SQLCA)
dw_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)

dw_tipoprodu.GetChild("tipr_codigo", dw_tipopro)
dw_tipopro.SetTransObject(sqlca)
dw_tipopro.Retrieve()
dw_tipoprodu.InsertRow(0)
dw_tipoprodu.SetItem(1, "tipr_codigo", 1)

em_fech_ini.Text			=	String(RelativeDate(Today(), -365))
em_fech_fin.Text			=	String(Today())

istr_mant.argumento[4]	=	String(gi_codexport)
istr_mant.argumento[1] 	=	"0"
istr_mant.argumento[2] 	=	em_fech_ini.Text
istr_mant.argumento[3] 	=	em_fech_fin.Text


end event

type st_computador from w_para_informes`st_computador within w_info_envasezonas
end type

type st_usuario from w_para_informes`st_usuario within w_info_envasezonas
end type

type st_temporada from w_para_informes`st_temporada within w_info_envasezonas
end type

type p_logo from w_para_informes`p_logo within w_info_envasezonas
end type

type st_titulo from w_para_informes`st_titulo within w_info_envasezonas
integer width = 1934
string text = "Producción de Envases por Zona"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_envasezonas
string tag = "Imprimir Reporte"
integer x = 2277
integer y = 1080
integer taborder = 50
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_cbx_productor,li_cliente, li_produ
String	l_s_titulo,ls_descri,texto_fecha
Date		ld_fech_ini,ld_fech_fin

l_s_titulo	= 'Producción'
istr_info.titulo	= 'Fruta Procesada '+l_s_titulo 

li_cliente=dw_2.GetITemNumber(1,'clie_codigo')

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_composite_envases_zonas"
vinf.dw_1.SetTransObject(sqlca)

ld_fech_ini 	= 	Date(istr_mant.argumento[2])
ld_fech_fin 	= 	Date(istr_mant.argumento[3])
texto_fecha		=	'Desde El:  ' + f_fecha_texto(String(ld_fech_ini, "dd/mm/yyyy"), 1) + &
						'         Hasta El:  ' + f_fecha_texto(String(ld_fech_fin, "dd/mm/yyyy"), 1)

IF cbx_productor.Checked THEN
	li_produ		=	0
	ls_descri	=	"Todos"	
ELSE
	li_produ 	= Integer(istr_mant.argumento[1])
	SELECT tipr_nombre
	INTO	:ls_descri
	FROM DBA.tipoproduc
	WHERE	tipr_codigo	=:li_produ;
END IF


fila = vinf.dw_1.Retrieve(li_cliente,li_produ,ld_fech_ini,ld_fech_fin)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe1.", &
	             StopSign!, Ok!)

 ELSE
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha+ "'")
		vinf.dw_1.Modify("tipo.text = '" + ls_descri+ "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_envasezonas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2272
integer y = 1348
integer taborder = 60
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_envasezonas
integer x = 242
integer y = 440
integer width = 1934
integer height = 588
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_tipoprodu from datawindow within w_info_envasezonas
integer x = 878
integer y = 832
integer width = 873
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_tipoproduc"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]=data
end event

type cbx_productor from checkbox within w_info_envasezonas
integer x = 878
integer y = 716
integer width = 279
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;call super::clicked;IF This.Checked THEN
	dw_tipoprodu.Enabled			= 	False
	pb_acepta.SetFocus()
	istr_mant.argumento[1] ="0"
ELSE
	dw_tipoprodu.Enabled			= 	True
	dw_tipoprodu.SetFocus()
	istr_mant.argumento[1] ="1"
END IF

end event

type st_1 from statictext within w_info_envasezonas
integer x = 338
integer y = 840
integer width = 539
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_envasezonas
integer x = 242
integer y = 1028
integer width = 1934
integer height = 504
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_envasezonas
integer x = 347
integer y = 1132
integer width = 375
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_envasezonas
integer x = 338
integer y = 1308
integer width = 453
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Fecha Término"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_envasezonas
integer x = 878
integer y = 1136
integer width = 393
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[2]=this.text
end event

type em_fech_fin from editmask within w_info_envasezonas
integer x = 878
integer y = 1312
integer width = 393
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[3]=this.text
end event

type dw_2 from datawindow within w_info_envasezonas
integer x = 878
integer y = 552
integer width = 1253
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[4]=data
end event

type st_6 from statictext within w_info_envasezonas
integer x = 338
integer y = 560
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

