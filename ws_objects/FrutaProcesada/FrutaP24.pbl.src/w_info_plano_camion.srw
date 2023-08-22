$PBExportHeader$w_info_plano_camion.srw
forward
global type w_info_plano_camion from w_para_informes
end type
type st_1 from statictext within w_info_plano_camion
end type
type st_2 from statictext within w_info_plano_camion
end type
type st_6 from statictext within w_info_plano_camion
end type
type st_4 from statictext within w_info_plano_camion
end type
type sle_conte from singlelineedit within w_info_plano_camion
end type
type st_3 from statictext within w_info_plano_camion
end type
type em_fecha from editmask within w_info_plano_camion
end type
type st_5 from statictext within w_info_plano_camion
end type
type sle_patente from singlelineedit within w_info_plano_camion
end type
type st_7 from statictext within w_info_plano_camion
end type
type sle_sag from singlelineedit within w_info_plano_camion
end type
type cb_1 from commandbutton within w_info_plano_camion
end type
type st_8 from statictext within w_info_plano_camion
end type
type dw_3 from datawindow within w_info_plano_camion
end type
type cbx_1 from checkbox within w_info_plano_camion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_plano_camion
end type
type uo_selplanta from uo_seleccion_plantas within w_info_plano_camion
end type
end forward

global type w_info_plano_camion from w_para_informes
integer x = 14
integer y = 32
integer width = 3397
integer height = 2204
string title = "Módulo de Despacho"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
st_6 st_6
st_4 st_4
sle_conte sle_conte
st_3 st_3
em_fecha em_fecha
st_5 st_5
sle_patente sle_patente
st_7 st_7
sle_sag sle_sag
cb_1 cb_1
st_8 st_8
dw_3 dw_3
cbx_1 cbx_1
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_plano_camion w_info_plano_camion

type variables
Integer	il_fila
end variables

forward prototypes
public function boolean existecontenedor (integer li_cliente, integer li_planta, string ls_contenedor)
end prototypes

public function boolean existecontenedor (integer li_cliente, integer li_planta, string ls_contenedor);integer li_cont

SELECT count(*) 
INTO :li_cont 
FROM dbo.DESPAFRIGOEN WHERE
    SUBSTRING(:ls_contenedor,1,10) in ('-1',SUBSTRING(defe_nrcont,1,10)) AND
    clie_codigo = :li_cliente AND
    plde_codigo = :li_planta;
	 
IF li_cont > 0 THEN
	return TRUE
ELSE
	return FALSE
END IF



end function

on w_info_plano_camion.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_6=create st_6
this.st_4=create st_4
this.sle_conte=create sle_conte
this.st_3=create st_3
this.em_fecha=create em_fecha
this.st_5=create st_5
this.sle_patente=create sle_patente
this.st_7=create st_7
this.sle_sag=create sle_sag
this.cb_1=create cb_1
this.st_8=create st_8
this.dw_3=create dw_3
this.cbx_1=create cbx_1
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.sle_conte
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.em_fecha
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.sle_patente
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.sle_sag
this.Control[iCurrent+12]=this.cb_1
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.dw_3
this.Control[iCurrent+15]=this.cbx_1
this.Control[iCurrent+16]=this.uo_selcliente
this.Control[iCurrent+17]=this.uo_selplanta
end on

on w_info_plano_camion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.sle_conte)
destroy(this.st_3)
destroy(this.em_fecha)
destroy(this.st_5)
destroy(this.sle_patente)
destroy(this.st_7)
destroy(this.sle_sag)
destroy(this.cb_1)
destroy(this.st_8)
destroy(this.dw_3)
destroy(this.cbx_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;Date 		ld_fecha
Boolean	lb_Cerrar = False

SetNull(ld_fecha)

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_codexport)
	uo_SelPlanta.Inicia(gi_codplanta)
	
	em_fecha.text					=	String(ld_fecha)	
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_plano_camion
integer x = 3045
integer y = 512
end type

type st_computador from w_para_informes`st_computador within w_info_plano_camion
end type

type st_usuario from w_para_informes`st_usuario within w_info_plano_camion
end type

type st_temporada from w_para_informes`st_temporada within w_info_plano_camion
end type

type p_logo from w_para_informes`p_logo within w_info_plano_camion
end type

type st_titulo from w_para_informes`st_titulo within w_info_plano_camion
integer x = 256
integer width = 2665
integer textsize = -11
string text = "Estiba Plano Camión"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_plano_camion
integer x = 3045
integer y = 820
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_fila, li_control
String		ls_nrconte, ls_patente, ls_sag
Date		ld_fecha

ls_nrconte	=	Mid(sle_conte.Text,1,10)

If il_fila > 0 Then
	If ls_nrconte = '' Then
		ls_nrconte = Mid(dw_3.Object.defe_nrcont[il_fila],1,10)
	End If	
End If	

ls_patente	=	sle_patente.Text
ls_sag		=	sle_sag.Text

If ls_nrconte = '' Then
	ls_nrconte = '-1'
End If
If ls_patente = '' Then
	ls_patente = '-1'
End If	
If ls_sag = '' Then
	ls_sag = '-1'	
End If	
If ls_nrconte = '-1' AND ls_patente = '-1' AND ls_sag = '-1' AND Date(em_fecha.text) = Date('1900-01-01') Then
	MessageBox( "Atención", "Seleccione un Contenedor, Patente, Planilla SAG o Fecha Despacho.", StopSign!, Ok!)
	Return	
End If	

If NOT existecontenedor(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ls_nrconte) Then
	MessageBox( "No Existe información", "No existe Contenedor.", StopSign!, Ok!)
	return				
End If	

istr_info.titulo	= 'Estiba Plano Camión'
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_plano_camion"
vinf.dw_1.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

ld_fecha = Date(em_fecha.text)

If dw_3.RowCount() = 0 Then
	li_fila = dw_3.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo,ls_nrconte,ld_fecha,ls_patente,ls_sag)
	
	If li_fila > 1 Then
		MessageBox( "Seleccion", "Seleccione un Registro para Imprimir.", StopSign!, Ok!)
		Return 1				
	End If
ElseIf il_fila = 0 Then
	MessageBox( "Seleccion", "Seleccione un Registro para Imprimir.", StopSign!, Ok!)
	Return 1		
End If

If dw_3.RowCount() > 1 AND il_fila <> 0 Then
	ls_nrconte 	= Mid(dw_3.Object.defe_nrcont[il_fila],1,10)
	ld_fecha 	= dw_3.Object.defe_fecdes[il_fila]
	ls_patente	= dw_3.Object.defe_patent[il_fila]
	ls_sag		= String(dw_3.Object.defe_plasag[il_fila])
End If	

If cbx_1.Checked Then
	li_control = 1
Else
	li_control = 0
End If	

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo,ls_nrconte,ld_fecha,ls_patente,ls_sag,li_control)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	dw_3.SelectRow(il_fila,False)
	
	If ls_patente	<> '-1' Then vinf.dw_1.ModIfy("t_patente.text = '" + ls_patente + "'")
	If ls_sag <> '-1' Then vinf.dw_1.ModIfy("t_sag.text = '" + ls_sag + "'")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_plano_camion
integer x = 3058
integer y = 1092
integer taborder = 50
end type

type st_1 from statictext within w_info_plano_camion
integer x = 718
integer y = 580
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_plano_camion
integer x = 722
integer y = 684
integer width = 489
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
string text = "Nro. Contenedor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_plano_camion
integer x = 718
integer y = 472
integer width = 233
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

type st_4 from statictext within w_info_plano_camion
integer x = 256
integer y = 440
integer width = 2665
integer height = 668
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_conte from singlelineedit within w_info_plano_camion
integer x = 1271
integer y = 660
integer width = 960
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;dw_3.Reset()
end event

type st_3 from statictext within w_info_plano_camion
integer x = 718
integer y = 996
integer width = 494
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
string text = "Fecha Despacho"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_plano_camion
integer x = 1271
integer y = 972
integer width = 402
integer height = 96
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;call super::modified;
dw_3.Reset()
end event

type st_5 from statictext within w_info_plano_camion
integer x = 718
integer y = 788
integer width = 489
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
string text = "Patente Camión"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_patente from singlelineedit within w_info_plano_camion
integer x = 1271
integer y = 760
integer width = 960
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;dw_3.Reset()
end event

type st_7 from statictext within w_info_plano_camion
integer x = 722
integer y = 892
integer width = 489
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
string text = "Planilla SAG"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_sag from singlelineedit within w_info_plano_camion
integer x = 1271
integer y = 860
integer width = 960
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;dw_3.Reset()
end event

type cb_1 from commandbutton within w_info_plano_camion
integer x = 1691
integer y = 976
integer width = 448
integer height = 92
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Limpia Fecha"
end type

event clicked;Date ld_fecha
SetNull(ld_fecha)

em_fecha.text					=	String(ld_fecha)	
end event

type st_8 from statictext within w_info_plano_camion
integer x = 256
integer y = 1112
integer width = 2665
integer height = 836
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_info_plano_camion
integer x = 297
integer y = 1140
integer width = 2583
integer height = 768
integer taborder = 190
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_estibacamiones"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type cbx_1 from checkbox within w_info_plano_camion
integer x = 2176
integer y = 996
integer width = 699
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
string text = "Incluye Temperatura"
boolean lefttext = true
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_plano_camion
event destroy ( )
integer x = 1271
integer y = 464
integer height = 96
integer taborder = 70
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;dw_3.Reset()	
end event

type uo_selplanta from uo_seleccion_plantas within w_info_plano_camion
integer x = 1271
integer y = 568
integer height = 96
integer taborder = 70
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;dw_3.Reset()	
end event

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

