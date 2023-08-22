$PBExportHeader$w_info_digitosag.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_info_digitosag from w_para_informes
end type
type st_1 from statictext within w_info_digitosag
end type
type dw_cliente from datawindow within w_info_digitosag
end type
type st_2 from statictext within w_info_digitosag
end type
type dw_plantadesp from datawindow within w_info_digitosag
end type
type st_4 from statictext within w_info_digitosag
end type
type ddlb_tipocond from dropdownlistbox within w_info_digitosag
end type
type em_numero from editmask within w_info_digitosag
end type
type st_3 from statictext within w_info_digitosag
end type
type st_7 from statictext within w_info_digitosag
end type
type dw_digitos from datawindow within w_info_digitosag
end type
type sle_digitos from singlelineedit within w_info_digitosag
end type
type st_5 from statictext within w_info_digitosag
end type
type st_6 from statictext within w_info_digitosag
end type
type cbx_rotulado from checkbox within w_info_digitosag
end type
end forward

global type w_info_digitosag from w_para_informes
integer width = 2542
integer height = 1588
string title = "SOLICITUD DE INSPECCION"
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
st_4 st_4
ddlb_tipocond ddlb_tipocond
em_numero em_numero
st_3 st_3
st_7 st_7
dw_digitos dw_digitos
sle_digitos sle_digitos
st_5 st_5
st_6 st_6
cbx_rotulado cbx_rotulado
end type
global w_info_digitosag w_info_digitosag

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas, dwc_digitos

Integer 				ii_Rotulado
end variables

on w_info_digitosag.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.st_4=create st_4
this.ddlb_tipocond=create ddlb_tipocond
this.em_numero=create em_numero
this.st_3=create st_3
this.st_7=create st_7
this.dw_digitos=create dw_digitos
this.sle_digitos=create sle_digitos
this.st_5=create st_5
this.st_6=create st_6
this.cbx_rotulado=create cbx_rotulado
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.ddlb_tipocond
this.Control[iCurrent+7]=this.em_numero
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.dw_digitos
this.Control[iCurrent+11]=this.sle_digitos
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.st_6
this.Control[iCurrent+14]=this.cbx_rotulado
end on

on w_info_digitosag.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.st_4)
destroy(this.ddlb_tipocond)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.dw_digitos)
destroy(this.sle_digitos)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.cbx_rotulado)
end on

event open;call super::open;dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

ddlb_tipocond.SelectItem(1)

dw_digitos.GetChild("compute_0001", dwc_digitos)
dwc_digitos.SetTransObject(Sqlca)
dwc_digitos.Retrieve(gi_CodExport,gi_CodPlanta,0,0)	//Digitos de Inspección
dw_digitos.InsertRow(0)

IF gi_prod_rotulado = 1 THEN
	cbx_rotulado.Checked	= True
	cbx_rotulado.Enabled	= False
ELSE
	cbx_rotulado.Checked	= False
	cbx_rotulado.Enabled	= True
END IF	

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)
istr_mant.Argumento[3]	=	'1'
istr_mant.Argumento[4]	=	'0'
istr_mant.Argumento[6]	=	''
ii_Rotulado 				= 	0
pb_acepta.Enabled	=	False
end event

type pb_excel from w_para_informes`pb_excel within w_info_digitosag
end type

type st_computador from w_para_informes`st_computador within w_info_digitosag
end type

type st_usuario from w_para_informes`st_usuario within w_info_digitosag
end type

type st_temporada from w_para_informes`st_temporada within w_info_digitosag
end type

type p_logo from w_para_informes`p_logo within w_info_digitosag
end type

type st_titulo from w_para_informes`st_titulo within w_info_digitosag
integer width = 1746
string text = "Emisión Pallets Dígito SAG"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_digitosag
string tag = "Imprimir Reporte"
integer x = 2126
integer y = 696
integer taborder = 60
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

IF em_numero.Text = "" THEN RETURN

lstr_info.titulo	= "SOLICITUD PALLETS POR DIGITO S.A.G."
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_digitosag"

IF cbx_rotulado.Checked THEN
	ii_Rotulado = 1
ELSE
	ii_Rotulado = 0
END IF

vinf.dw_1.SetTransObject(sqlca)

ll_Filas = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
										Integer(istr_mant.argumento[2]), &
										Integer(istr_mant.argumento[3]), &
										Long(istr_mant.argumento[4]), &
										istr_mant.argumento[16],ii_rotulado)
								  
IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
//	vinf.dw_1.Object.dw_detalle.Object.nom_empresa.text	= gstr_apl.nom_empresa
//	vinf.dw_1.Object.dw_detalle.Object.rut_empresa.text	= 'R.U.T. ' + String(Double(Mid(gstr_apl.rut_empresa,1,9)),'000,000,000') + '-' + Mid(gstr_apl.rut_empresa,10,1)
//	vinf.dw_1.Object.dw_detalle.Object.dir_empresa.text	= gstr_apl.dir_empresa
//
   F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("digitos.text = '" + istr_mant.Argumento[6] + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_digitosag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2126
integer y = 1032
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_digitosag
integer x = 288
integer y = 496
integer width = 347
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
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_info_digitosag
integer x = 750
integer y = 480
integer width = 1207
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
	dwc_digitos.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]))
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_digitosag
integer x = 288
integer y = 624
integer width = 347
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_info_digitosag
integer x = 750
integer y = 608
integer width = 1024
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
	dwc_digitos.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]))
ELSE
	RETURN 1
END IF
end event

type st_4 from statictext within w_info_digitosag
integer x = 288
integer y = 752
integer width = 334
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
string text = "Condición"
boolean focusrectangle = false
end type

type ddlb_tipocond from dropdownlistbox within w_info_digitosag
integer x = 750
integer y = 736
integer width = 526
integer height = 292
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
boolean sorted = false
string item[] = {"Inspección","Re-Inspección"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
dwc_digitos.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]))
end event

type em_numero from editmask within w_info_digitosag
integer x = 750
integer y = 868
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text
dwc_digitos.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]))
end event

type st_3 from statictext within w_info_digitosag
integer x = 288
integer y = 880
integer width = 251
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
string text = "Número"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_digitosag
integer x = 288
integer y = 1008
integer width = 251
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
string text = "Dígitos"
boolean focusrectangle = false
end type

type dw_digitos from datawindow within w_info_digitosag
integer x = 750
integer y = 992
integer width = 329
integer height = 96
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_digitosag"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long	ll_digito

istr_mant.argumento[5] = data


ll_digito	=	Pos(istr_mant.argumento[6], istr_mant.argumento[5])

IF ll_digito = 0 THEN
	istr_mant.argumento[6] = data+' '+istr_mant.argumento[6]
	istr_mant.argumento[16] = istr_mant.argumento[16]+data
	
	sle_digitos.text = istr_mant.argumento[6]
END IF

IF istr_mant.argumento[6] <> '' THEN
	pb_acepta.Enabled	=	True
END IF
end event

type sle_digitos from singlelineedit within w_info_digitosag
integer x = 1083
integer y = 996
integer width = 827
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_info_digitosag
integer x = 251
integer y = 440
integer width = 1746
integer height = 816
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_digitosag
integer x = 288
integer y = 1136
integer width = 430
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
string text = "Prod Rotulado"
boolean focusrectangle = false
end type

type cbx_rotulado from checkbox within w_info_digitosag
integer x = 750
integer y = 1140
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

