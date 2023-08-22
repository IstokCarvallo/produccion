$PBExportHeader$w_info_planilla_despacho_muestras.srw
forward
global type w_info_planilla_despacho_muestras from w_para_informes
end type
type st_1 from statictext within w_info_planilla_despacho_muestras
end type
type st_2 from statictext within w_info_planilla_despacho_muestras
end type
type st_6 from statictext within w_info_planilla_despacho_muestras
end type
type em_nroguia from editmask within w_info_planilla_despacho_muestras
end type
type dw_guia from datawindow within w_info_planilla_despacho_muestras
end type
type st_11 from statictext within w_info_planilla_despacho_muestras
end type
type st_12 from statictext within w_info_planilla_despacho_muestras
end type
type st_13 from statictext within w_info_planilla_despacho_muestras
end type
type st_20 from statictext within w_info_planilla_despacho_muestras
end type
type st_14 from statictext within w_info_planilla_despacho_muestras
end type
type mle_numsellos from multilineedit within w_info_planilla_despacho_muestras
end type
type em_cantsellos from editmask within w_info_planilla_despacho_muestras
end type
type em_pallet from editmask within w_info_planilla_despacho_muestras
end type
type em_unidades from editmask within w_info_planilla_despacho_muestras
end type
type em_sitio from editmask within w_info_planilla_despacho_muestras
end type
type st_3 from statictext within w_info_planilla_despacho_muestras
end type
type mle_contraparte from multilineedit within w_info_planilla_despacho_muestras
end type
type st_4 from statictext within w_info_planilla_despacho_muestras
end type
type st_5 from statictext within w_info_planilla_despacho_muestras
end type
type mle_observaciones from multilineedit within w_info_planilla_despacho_muestras
end type
type cbx_todos from checkbox within w_info_planilla_despacho_muestras
end type
type st_7 from statictext within w_info_planilla_despacho_muestras
end type
type em_lote from editmask within w_info_planilla_despacho_muestras
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_planilla_despacho_muestras
end type
type uo_selplanta from uo_seleccion_plantas within w_info_planilla_despacho_muestras
end type
end forward

global type w_info_planilla_despacho_muestras from w_para_informes
integer x = 14
integer y = 32
integer width = 3653
integer height = 1812
string title = "Emisión Planilla de Despacho Muestra Lotes Inspección"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
st_6 st_6
em_nroguia em_nroguia
dw_guia dw_guia
st_11 st_11
st_12 st_12
st_13 st_13
st_20 st_20
st_14 st_14
mle_numsellos mle_numsellos
em_cantsellos em_cantsellos
em_pallet em_pallet
em_unidades em_unidades
em_sitio em_sitio
st_3 st_3
mle_contraparte mle_contraparte
st_4 st_4
st_5 st_5
mle_observaciones mle_observaciones
cbx_todos cbx_todos
st_7 st_7
em_lote em_lote
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_planilla_despacho_muestras w_info_planilla_despacho_muestras

type variables
Integer	ii_tipo
integer  	ii_var, ii_prod, ii_cal, ii_cli, ii_pakrot
end variables

forward prototypes
public function boolean existeguia (long al_guia)
end prototypes

public function boolean existeguia (long al_guia);Integer	li_Tipova, li_tipoembq, li_embc_codigo, li_tecnic, sire_codigo
Date		ld_fecha
String		ls_Embarque, ls_puerto, ls_destino, ls_contraparte, ls_ciudad, ls_direccion, ls_rut, ls_nombre
Long		ll_cantidad, ll_despacho

If al_guia <> 0 OR uo_SelPlanta.Codigo = 0 Then

	SELECT max(defe_tiposa)
		INTO	:li_tipoembq
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:uo_SelPlanta.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_guides	=	:al_guia;
				
	If sqlca.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_nroguia.SetFocus()
		Return False
	ElseIf sqlca.SQLCode = 100 Then
		MessageBox("Atención", "No existe Número Guia Despacho Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		em_nroguia.SetFocus()
		Return False
	Else		
		SELECT embq_codigo, defe_tecnic,defe_numero,sire_codigo
		INTO	:ls_Embarque, :li_tecnic, :ll_despacho,:sire_codigo
		FROM	dbo.DESPAFRIGOEN
		WHERE	plde_codigo =	:uo_SelPlanta.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_guides	=	:al_guia
		AND	defe_tiposa =	32;
		
		SELECT sum(defi_cantid)
		INTO	:ll_cantidad
		FROM	dbo.DESPAFRIGOinfo
		WHERE	plde_codigo =	:uo_SelPlanta.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_numero	=	:ll_despacho;
		
	SELECT sit.sire_nombre,ciu.ciud_nombre,sire_direcc,sit.sire_codrut 
	INTO	:ls_nombre,:ls_ciudad,:ls_direccion,:ls_rut  
    	FROM dbo.sitiorevision as sit,dbo.ciudad as ciu,dbo.provincias as pro  
   	WHERE ciu.ciud_codigo = sit.ciud_codigo and  
         ciu.regi_codigo = sit.regi_codigo and  
         ciu.prov_codigo = sit.prov_codigo and  
         ciu.regi_codigo = pro.regi_codigo and  
         ciu.prov_codigo = pro.prov_codigo and
			sit.sire_codigo = :sire_codigo;

		em_unidades.Text = String(ll_cantidad)

		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
			em_nroguia.SetFocus()
			Return False
		ElseIf sqlca.SQLCode = 100 Then
				MessageBox("Atención", "No existe Guía.~r~r", Exclamation!, Ok!)
				em_nroguia.SetFocus()
				Return False
		Else
			If ls_nombre <> '' Then em_sitio.Text = ls_nombre
		End If
	End If	
End If		
end function

on w_info_planilla_despacho_muestras.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_6=create st_6
this.em_nroguia=create em_nroguia
this.dw_guia=create dw_guia
this.st_11=create st_11
this.st_12=create st_12
this.st_13=create st_13
this.st_20=create st_20
this.st_14=create st_14
this.mle_numsellos=create mle_numsellos
this.em_cantsellos=create em_cantsellos
this.em_pallet=create em_pallet
this.em_unidades=create em_unidades
this.em_sitio=create em_sitio
this.st_3=create st_3
this.mle_contraparte=create mle_contraparte
this.st_4=create st_4
this.st_5=create st_5
this.mle_observaciones=create mle_observaciones
this.cbx_todos=create cbx_todos
this.st_7=create st_7
this.em_lote=create em_lote
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.em_nroguia
this.Control[iCurrent+5]=this.dw_guia
this.Control[iCurrent+6]=this.st_11
this.Control[iCurrent+7]=this.st_12
this.Control[iCurrent+8]=this.st_13
this.Control[iCurrent+9]=this.st_20
this.Control[iCurrent+10]=this.st_14
this.Control[iCurrent+11]=this.mle_numsellos
this.Control[iCurrent+12]=this.em_cantsellos
this.Control[iCurrent+13]=this.em_pallet
this.Control[iCurrent+14]=this.em_unidades
this.Control[iCurrent+15]=this.em_sitio
this.Control[iCurrent+16]=this.st_3
this.Control[iCurrent+17]=this.mle_contraparte
this.Control[iCurrent+18]=this.st_4
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.mle_observaciones
this.Control[iCurrent+21]=this.cbx_todos
this.Control[iCurrent+22]=this.st_7
this.Control[iCurrent+23]=this.em_lote
this.Control[iCurrent+24]=this.uo_selcliente
this.Control[iCurrent+25]=this.uo_selplanta
end on

on w_info_planilla_despacho_muestras.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.em_nroguia)
destroy(this.dw_guia)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.st_20)
destroy(this.st_14)
destroy(this.mle_numsellos)
destroy(this.em_cantsellos)
destroy(this.em_pallet)
destroy(this.em_unidades)
destroy(this.em_sitio)
destroy(this.st_3)
destroy(this.mle_contraparte)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.mle_observaciones)
destroy(this.cbx_todos)
destroy(this.st_7)
destroy(this.em_lote)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelPlanta.Codigo) Then  lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then  lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	uo_SelPlanta.Inicia(gi_CodPlanta)	
	
	ii_tipo	=	Integer(Message.StringParm)
	dw_guia.DataObject	= 'dw_info_planilladespacho_muestras'		
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_planilla_despacho_muestras
integer x = 3232
integer y = 564
end type

type st_computador from w_para_informes`st_computador within w_info_planilla_despacho_muestras
end type

type st_usuario from w_para_informes`st_usuario within w_info_planilla_despacho_muestras
end type

type st_temporada from w_para_informes`st_temporada within w_info_planilla_despacho_muestras
end type

type p_logo from w_para_informes`p_logo within w_info_planilla_despacho_muestras
end type

type st_titulo from w_para_informes`st_titulo within w_info_planilla_despacho_muestras
integer width = 2857
string text = "Emisión Planilla de Despacho Muestras Lote Inspección"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planilla_despacho_muestras
integer x = 3223
integer y = 1052
integer taborder = 120
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Long		ll_Fila
String		ls_lotes

istr_info.titulo	= 'EMISION PLANILLA DE DESPACHO'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_planilladespacho_muestras"
//vinf.dw_1.DataObject = "dw_info_planilladespacho_muestras_nuevo"
vinf.dw_1.SetTransObject(sqlca)

IF cbx_todos.Checked THEN
	ls_lotes = '-1'
ELSE	
	ls_lotes = em_lote.Text
END IF	

ll_Fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Long(em_nroguia.Text),ls_lotes)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("t_sitio.text = '" + em_sitio.text + "'")
	vinf.dw_1.Modify("t_unidades.text = '" + em_unidades.text + "'")
	vinf.dw_1.Modify("t_pallet.text = '" + em_pallet.text + "'")
	vinf.dw_1.Modify("t_cantsellos.text = '" + em_cantsellos.text + "'")
	vinf.dw_1.Modify("t_numsellos.text = '" + mle_numsellos.text + "'")
	vinf.dw_1.Modify("t_nominspector.text = '" + mle_contraparte.text + "'")	
	vinf.dw_1.Modify("t_observaciones.text = '" + mle_observaciones.text + "'")
	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	vinf.dw_1.GroupCalc()
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_planilla_despacho_muestras
integer x = 3223
integer y = 1320
integer taborder = 130
end type

type st_1 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 564
integer width = 462
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 672
integer width = 325
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
string text = "Nro. Guia"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 456
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

type em_nroguia from editmask within w_info_planilla_despacho_muestras
integer x = 1042
integer y = 652
integer width = 402
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF ExisteGuia(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type dw_guia from datawindow within w_info_planilla_despacho_muestras
boolean visible = false
integer x = 1422
integer y = 1972
integer width = 686
integer height = 400
integer taborder = 40
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_11 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 780
integer width = 763
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
string text = "A Sitio Inspección"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 888
integer width = 658
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
boolean enabled = false
string text = "Cantidad Unidades"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 996
integer width = 658
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
boolean enabled = false
string text = "Cantidad Pallets"
boolean focusrectangle = false
end type

type st_20 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 1104
integer width = 658
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
boolean enabled = false
string text = "Cantidad Sellos"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 1212
integer width = 635
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
boolean enabled = false
string text = "N° de los Sellos"
boolean focusrectangle = false
end type

type mle_numsellos from multilineedit within w_info_planilla_despacho_muestras
integer x = 1042
integer y = 1192
integer width = 1582
integer height = 200
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
end type

type em_cantsellos from editmask within w_info_planilla_despacho_muestras
event getfocus pbm_ensetfocus
integer x = 1042
integer y = 1084
integer width = 448
integer height = 84
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type em_pallet from editmask within w_info_planilla_despacho_muestras
event getfocus pbm_ensetfocus
integer x = 1042
integer y = 976
integer width = 448
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type em_unidades from editmask within w_info_planilla_despacho_muestras
event getfocus pbm_ensetfocus
integer x = 1042
integer y = 868
integer width = 448
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type em_sitio from editmask within w_info_planilla_despacho_muestras
event getfocus pbm_ensetfocus
integer x = 1042
integer y = 760
integer width = 1582
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_3 from statictext within w_info_planilla_despacho_muestras
boolean visible = false
integer x = 302
integer y = 1892
integer width = 635
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Contraparte"
boolean focusrectangle = false
end type

type mle_contraparte from multilineedit within w_info_planilla_despacho_muestras
boolean visible = false
integer x = 1051
integer y = 1872
integer width = 1952
integer height = 84
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
end type

type st_4 from statictext within w_info_planilla_despacho_muestras
integer x = 251
integer y = 412
integer width = 2857
integer height = 1268
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

type st_5 from statictext within w_info_planilla_despacho_muestras
integer x = 302
integer y = 1416
integer width = 635
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
boolean enabled = false
string text = "Observaciones"
boolean focusrectangle = false
end type

type mle_observaciones from multilineedit within w_info_planilla_despacho_muestras
integer x = 1042
integer y = 1412
integer width = 1952
integer height = 232
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cbx_todos from checkbox within w_info_planilla_despacho_muestras
integer x = 2624
integer y = 652
integer width = 416
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
string text = "Todos"
boolean checked = true
borderstyle borderstyle = styleraised!
end type

event clicked;IF This.Checked THEN
	em_lote.Enabled = False
	em_lote.Text = ''
ELSE
	em_lote.Enabled = True
	em_lote.Text = ''
END IF	
end event

type st_7 from statictext within w_info_planilla_despacho_muestras
integer x = 1545
integer y = 672
integer width = 242
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
string text = "Lotes"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_lote from editmask within w_info_planilla_despacho_muestras
integer x = 1801
integer y = 656
integer width = 791
integer height = 84
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_planilla_despacho_muestras
event destroy ( )
integer x = 1042
integer y = 440
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_planilla_despacho_muestras
event destroy ( )
integer x = 1042
integer y = 552
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

