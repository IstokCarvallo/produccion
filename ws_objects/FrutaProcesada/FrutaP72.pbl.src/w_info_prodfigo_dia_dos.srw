$PBExportHeader$w_info_prodfigo_dia_dos.srw
forward
global type w_info_prodfigo_dia_dos from w_para_informes
end type
type st_4 from statictext within w_info_prodfigo_dia_dos
end type
type cbx_productor from checkbox within w_info_prodfigo_dia_dos
end type
type st_1 from statictext within w_info_prodfigo_dia_dos
end type
type st_5 from statictext within w_info_prodfigo_dia_dos
end type
type st_2 from statictext within w_info_prodfigo_dia_dos
end type
type em_fech_ini from editmask within w_info_prodfigo_dia_dos
end type
type dw_2 from datawindow within w_info_prodfigo_dia_dos
end type
type st_6 from statictext within w_info_prodfigo_dia_dos
end type
type dw_1 from datawindow within w_info_prodfigo_dia_dos
end type
end forward

global type w_info_prodfigo_dia_dos from w_para_informes
integer x = 14
integer y = 32
integer width = 2702
integer height = 1616
string title = "Producción en Frigorificos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
cbx_productor cbx_productor
st_1 st_1
st_5 st_5
st_2 st_2
em_fech_ini em_fech_ini
dw_2 dw_2
st_6 st_6
dw_1 dw_1
end type
global w_info_prodfigo_dia_dos w_info_prodfigo_dia_dos

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_productores
end variables

on w_info_prodfigo_dia_dos.create
int iCurrent
call super::create
this.st_4=create st_4
this.cbx_productor=create cbx_productor
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_fech_ini=create em_fech_ini
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.cbx_productor
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_fech_ini
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.dw_1
end on

on w_info_prodfigo_dia_dos.destroy
call super::destroy
destroy(this.st_4)
destroy(this.cbx_productor)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_fech_ini)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
end on

event open;call super::open;dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)

dw_1.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
idwc_productores.Retrieve()
dw_1.InsertRow(0)
dw_1.Enabled	=	False

em_fech_ini.Text			=	String(Today())

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	"0"
istr_mant.argumento[3]	=	String(Today())


end event

type st_computador from w_para_informes`st_computador within w_info_prodfigo_dia_dos
end type

type st_usuario from w_para_informes`st_usuario within w_info_prodfigo_dia_dos
end type

type st_temporada from w_para_informes`st_temporada within w_info_prodfigo_dia_dos
end type

type p_logo from w_para_informes`p_logo within w_info_prodfigo_dia_dos
end type

type st_titulo from w_para_informes`st_titulo within w_info_prodfigo_dia_dos
integer width = 1934
string text = "Informe de Producción en Frigorificos diario"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_prodfigo_dia_dos
string tag = "Imprimir Reporte"
integer x = 2331
integer y = 880
integer taborder = 50
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_cbx_productor,li_cliente
String	l_s_titulo,ls_descri,texto_fecha
Date		ld_fecha_dia
Long		ll_produ

l_s_titulo	= 'Producción'
istr_info.titulo	= 'Fruta Procesada '+l_s_titulo 

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_comp_prodfigo_dia_dos"
vinf.dw_1.SetTransObject(sqlca)

ld_fecha_dia 	=	Date(istr_mant.argumento[3])
texto_fecha		=	'Fecha Embalaje:      '+String(ld_fecha_dia, "dd/mm/yyyy")

li_cliente 		=Integer(istr_mant.argumento[1])

IF cbx_productor.Checked THEN
	ll_produ		=	0
	ls_descri	=	"Todos"	
ELSE
     ll_produ 	= Long(istr_mant.argumento[2])
	  SELECT	prod_nombre  
	    INTO :ls_descri 
  		 FROM  dba.productores  
		 WHERE prod_codigo = :ll_produ;
END IF

fila = vinf.dw_1.Retrieve(ll_produ,li_cliente,ld_fecha_dia)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
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

type pb_salir from w_para_informes`pb_salir within w_info_prodfigo_dia_dos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2327
integer y = 1160
integer taborder = 60
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_prodfigo_dia_dos
integer x = 247
integer y = 440
integer width = 1934
integer height = 592
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

type cbx_productor from checkbox within w_info_prodfigo_dia_dos
integer x = 850
integer y = 752
integer width = 279
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_1.Enabled			= 	False
	pb_acepta.SetFocus()
	istr_mant.argumento[2]="0"

ELSE
	dw_1.Enabled			= 	True
	dw_1.SetFocus()
	istr_mant.argumento[2]="0"
END IF

end event

type st_1 from statictext within w_info_prodfigo_dia_dos
integer x = 343
integer y = 844
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_prodfigo_dia_dos
integer x = 247
integer y = 1032
integer width = 1934
integer height = 308
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

type st_2 from statictext within w_info_prodfigo_dia_dos
integer x = 352
integer y = 1136
integer width = 485
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Embalaje"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_prodfigo_dia_dos
integer x = 850
integer y = 1124
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

event modified;istr_mant.argumento[3]=this.text
end event

type dw_2 from datawindow within w_info_prodfigo_dia_dos
integer x = 850
integer y = 556
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data
idwc_productores.Retrieve()

end event

type st_6 from statictext within w_info_prodfigo_dia_dos
integer x = 343
integer y = 564
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_prodfigo_dia_dos
integer x = 850
integer y = 840
integer width = 1111
integer height = 104
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]=data
end event

