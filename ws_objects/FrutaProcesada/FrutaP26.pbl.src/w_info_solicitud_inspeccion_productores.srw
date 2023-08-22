$PBExportHeader$w_info_solicitud_inspeccion_productores.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_info_solicitud_inspeccion_productores from w_para_informes
end type
type st_1 from statictext within w_info_solicitud_inspeccion_productores
end type
type st_2 from statictext within w_info_solicitud_inspeccion_productores
end type
type st_4 from statictext within w_info_solicitud_inspeccion_productores
end type
type ddlb_tipocond from dropdownlistbox within w_info_solicitud_inspeccion_productores
end type
type em_numero from editmask within w_info_solicitud_inspeccion_productores
end type
type st_3 from statictext within w_info_solicitud_inspeccion_productores
end type
type st_6 from statictext within w_info_solicitud_inspeccion_productores
end type
type st_7 from statictext within w_info_solicitud_inspeccion_productores
end type
type em_sag from editmask within w_info_solicitud_inspeccion_productores
end type
type em_1 from editmask within w_info_solicitud_inspeccion_productores
end type
type st_5 from statictext within w_info_solicitud_inspeccion_productores
end type
type cbx_nuevo from checkbox within w_info_solicitud_inspeccion_productores
end type
type st_8 from statictext within w_info_solicitud_inspeccion_productores
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_solicitud_inspeccion_productores
end type
type uo_selplantas from uo_seleccion_plantas within w_info_solicitud_inspeccion_productores
end type
end forward

global type w_info_solicitud_inspeccion_productores from w_para_informes
integer width = 2807
integer height = 1712
string title = "LISTADO POR PRODUCTOR"
st_1 st_1
st_2 st_2
st_4 st_4
ddlb_tipocond ddlb_tipocond
em_numero em_numero
st_3 st_3
st_6 st_6
st_7 st_7
em_sag em_sag
em_1 em_1
st_5 st_5
cbx_nuevo cbx_nuevo
st_8 st_8
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_info_solicitud_inspeccion_productores w_info_solicitud_inspeccion_productores

type variables
Str_mant				istr_mant
end variables

forward prototypes
public function boolean existe_inspeccion (long ai_numero)
end prototypes

public function boolean existe_inspeccion (long ai_numero);Integer	li_cliente, li_planta, li_tipo		
Long		ll_count

li_tipo 	= Integer(istr_mant.argumento[3])

SELECT count(*) 
INTO :ll_count
FROM dbo.inspecpaldet
WHERE clie_codigo = :uo_SelCliente.Codigo
AND	plde_codigo = :uo_SelPlantas.Codigo
AND	inpe_tipoin = :li_tipo
AND	inpe_numero = :ai_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpaldet")
	RETURN True
ELSEIF ll_count = 0 THEN
	MessageBox("Atención", "Número de Inspección no ha sido Ingresado o NO tiene Detalle.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
	RETURN True
END IF	
					
Return False

end function

on w_info_solicitud_inspeccion_productores.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_4=create st_4
this.ddlb_tipocond=create ddlb_tipocond
this.em_numero=create em_numero
this.st_3=create st_3
this.st_6=create st_6
this.st_7=create st_7
this.em_sag=create em_sag
this.em_1=create em_1
this.st_5=create st_5
this.cbx_nuevo=create cbx_nuevo
this.st_8=create st_8
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.ddlb_tipocond
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_sag
this.Control[iCurrent+10]=this.em_1
this.Control[iCurrent+11]=this.st_5
this.Control[iCurrent+12]=this.cbx_nuevo
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.uo_selplantas
end on

on w_info_solicitud_inspeccion_productores.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.ddlb_tipocond)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.em_sag)
destroy(this.em_1)
destroy(this.st_5)
destroy(this.cbx_nuevo)
destroy(this.st_8)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelPlantas.Filtra(1)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	ddlb_tipocond.SelectItem(1)
	istr_mant.Argumento[3]	=	"1"
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_solicitud_inspeccion_productores
end type

type st_computador from w_para_informes`st_computador within w_info_solicitud_inspeccion_productores
end type

type st_usuario from w_para_informes`st_usuario within w_info_solicitud_inspeccion_productores
end type

type st_temporada from w_para_informes`st_temporada within w_info_solicitud_inspeccion_productores
end type

type p_logo from w_para_informes`p_logo within w_info_solicitud_inspeccion_productores
end type

type st_titulo from w_para_informes`st_titulo within w_info_solicitud_inspeccion_productores
integer width = 1989
string text = "Listado de Productores por Inspección"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_solicitud_inspeccion_productores
integer x = 2400
integer y = 832
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Filas, li_prdrot, li_calrot
Integer	li_varrot, li_Cliente, li_info

str_info	lstr_info

IF em_numero.Text = "" THEN RETURN

lstr_info.titulo	= "LISTADO DE PRODUCTORES POR INSPECCIÓN"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

IF cbx_nuevo.Checked THEN
	vinf.dw_1.DataObject = "dw_info_inpe_prod_nuevo"
ELSE
	vinf.dw_1.DataObject = "dw_info_inpe_prod"	
END IF	

IF em_numero.Text = '' THEN
	MessageBox("Atención", "Falta Número.~r~r" + &
					"Ingrese Número Válido.", Exclamation!, Ok!)
	em_numero.SetFocus()					
	RETURN
END IF	

IF em_sag.Text = '' THEN
	MessageBox("Atención", "Falta Correlativo SAG.~r~r" + &
					"Ingrese Correlativo SAG.", Exclamation!, Ok!)
	em_sag.SetFocus()					
	RETURN
END IF	

vinf.dw_1.SetTransObject(sqlca)


ll_Filas = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo, &
										Long(istr_mant.argumento[4]),Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[5]))
								  
IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	IF em_1.Text <> '' THEN
		vinf.dw_1.Object.nombre.text = String(istr_mant.argumento[6])
		vinf.dw_1.Object.exportadora.text	= gstr_apl.razon_social +' '+gstr_apl.Oficina
		vinf.dw_1.Object.sag.text	= 'Contraparte SAG'
	END IF	

	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_solicitud_inspeccion_productores
integer x = 2400
integer y = 1108
integer taborder = 80
end type

type st_1 from statictext within w_info_solicitud_inspeccion_productores
integer x = 343
integer y = 520
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

type st_2 from statictext within w_info_solicitud_inspeccion_productores
integer x = 343
integer y = 656
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

type st_4 from statictext within w_info_solicitud_inspeccion_productores
integer x = 343
integer y = 792
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

type ddlb_tipocond from dropdownlistbox within w_info_solicitud_inspeccion_productores
integer x = 727
integer y = 784
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
end event

type em_numero from editmask within w_info_solicitud_inspeccion_productores
integer x = 727
integer y = 924
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

event modified;
IF existe_inspeccion(Long(This.Text)) THEN
	This.Text = ''
	This.SetFocus()
ELSE
	istr_mant.argumento[4]	=	This.Text
END IF	
end event

type st_3 from statictext within w_info_solicitud_inspeccion_productores
integer x = 343
integer y = 928
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

type st_6 from statictext within w_info_solicitud_inspeccion_productores
integer x = 343
integer y = 1068
integer width = 498
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
string text = "Correlativo SAG."
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_solicitud_inspeccion_productores
integer x = 343
integer y = 1208
integer width = 379
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
string text = "Contraparte"
boolean focusrectangle = false
end type

type em_sag from editmask within w_info_solicitud_inspeccion_productores
integer x = 855
integer y = 1060
integer width = 407
integer height = 92
integer taborder = 50
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

event modified;istr_mant.argumento[5]	=	This.Text
end event

type em_1 from editmask within w_info_solicitud_inspeccion_productores
integer x = 727
integer y = 1200
integer width = 1486
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;istr_mant.argumento[6]	=	This.Text
end event

type st_5 from statictext within w_info_solicitud_inspeccion_productores
integer x = 251
integer y = 440
integer width = 1989
integer height = 876
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

type cbx_nuevo from checkbox within w_info_solicitud_inspeccion_productores
integer x = 846
integer y = 1336
integer width = 759
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Formato Nuevo"
end type

type st_8 from statictext within w_info_solicitud_inspeccion_productores
integer x = 251
integer y = 1316
integer width = 1989
integer height = 124
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_solicitud_inspeccion_productores
integer x = 727
integer y = 504
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_solicitud_inspeccion_productores
integer x = 727
integer y = 640
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

