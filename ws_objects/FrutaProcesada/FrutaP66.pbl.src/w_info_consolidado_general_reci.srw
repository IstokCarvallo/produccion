$PBExportHeader$w_info_consolidado_general_reci.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_consolidado_general_reci from w_para_informes
end type
type st_1 from statictext within w_info_consolidado_general_reci
end type
type st_2 from statictext within w_info_consolidado_general_reci
end type
type dw_embarques from datawindow within w_info_consolidado_general_reci
end type
type st_3 from statictext within w_info_consolidado_general_reci
end type
type st_4 from statictext within w_info_consolidado_general_reci
end type
type em_fzarpe from editmask within w_info_consolidado_general_reci
end type
type st_7 from statictext within w_info_consolidado_general_reci
end type
type st_8 from statictext within w_info_consolidado_general_reci
end type
type st_10 from statictext within w_info_consolidado_general_reci
end type
type dw_operaciones from datawindow within w_info_consolidado_general_reci
end type
type cbx_operacion from checkbox within w_info_consolidado_general_reci
end type
type cbx_embarque from checkbox within w_info_consolidado_general_reci
end type
type st_12 from statictext within w_info_consolidado_general_reci
end type
type cbx_recibidor from checkbox within w_info_consolidado_general_reci
end type
type st_15 from statictext within w_info_consolidado_general_reci
end type
type em_hasta from editmask within w_info_consolidado_general_reci
end type
type st_14 from statictext within w_info_consolidado_general_reci
end type
type st_5 from statictext within w_info_consolidado_general_reci
end type
type st_9 from statictext within w_info_consolidado_general_reci
end type
type st_6 from statictext within w_info_consolidado_general_reci
end type
type st_13 from statictext within w_info_consolidado_general_reci
end type
type uo_selespecie from uo_seleccion_especie within w_info_consolidado_general_reci
end type
type cbx_varirotula from checkbox within w_info_consolidado_general_reci
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_consolidado_general_reci
end type
type st_11 from statictext within w_info_consolidado_general_reci
end type
type st_17 from statictext within w_info_consolidado_general_reci
end type
type cbx_cantpallet from checkbox within w_info_consolidado_general_reci
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_consolidado_general_reci
end type
type uo_selplantas from uo_seleccion_plantas within w_info_consolidado_general_reci
end type
type uo_selproductor from uo_seleccion_productor within w_info_consolidado_general_reci
end type
type uo_seltransportista from uo_seleccion_transportista within w_info_consolidado_general_reci
end type
type uo_seltipocamion from uo_seleccion_tipocamion within w_info_consolidado_general_reci
end type
end forward

global type w_info_consolidado_general_reci from w_para_informes
integer width = 3954
integer height = 2040
st_1 st_1
st_2 st_2
dw_embarques dw_embarques
st_3 st_3
st_4 st_4
em_fzarpe em_fzarpe
st_7 st_7
st_8 st_8
st_10 st_10
dw_operaciones dw_operaciones
cbx_operacion cbx_operacion
cbx_embarque cbx_embarque
st_12 st_12
cbx_recibidor cbx_recibidor
st_15 st_15
em_hasta em_hasta
st_14 st_14
st_5 st_5
st_9 st_9
st_6 st_6
st_13 st_13
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
uo_selcondicion uo_selcondicion
st_11 st_11
st_17 st_17
cbx_cantpallet cbx_cantpallet
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
uo_selproductor uo_selproductor
uo_seltransportista uo_seltransportista
uo_seltipocamion uo_seltipocamion
end type
global w_info_consolidado_general_reci w_info_consolidado_general_reci

type variables
DataWindowChild 	idwc_embarque,idwc_operaciones

Integer	ii_Operacion
String		is_Embarque, is_NomEmbarque, is_NomNave, is_OPeracion



end variables

on w_info_consolidado_general_reci.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.dw_embarques=create dw_embarques
this.st_3=create st_3
this.st_4=create st_4
this.em_fzarpe=create em_fzarpe
this.st_7=create st_7
this.st_8=create st_8
this.st_10=create st_10
this.dw_operaciones=create dw_operaciones
this.cbx_operacion=create cbx_operacion
this.cbx_embarque=create cbx_embarque
this.st_12=create st_12
this.cbx_recibidor=create cbx_recibidor
this.st_15=create st_15
this.em_hasta=create em_hasta
this.st_14=create st_14
this.st_5=create st_5
this.st_9=create st_9
this.st_6=create st_6
this.st_13=create st_13
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.uo_selcondicion=create uo_selcondicion
this.st_11=create st_11
this.st_17=create st_17
this.cbx_cantpallet=create cbx_cantpallet
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
this.uo_selproductor=create uo_selproductor
this.uo_seltransportista=create uo_seltransportista
this.uo_seltipocamion=create uo_seltipocamion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.dw_embarques
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.em_fzarpe
this.Control[iCurrent+7]=this.st_7
this.Control[iCurrent+8]=this.st_8
this.Control[iCurrent+9]=this.st_10
this.Control[iCurrent+10]=this.dw_operaciones
this.Control[iCurrent+11]=this.cbx_operacion
this.Control[iCurrent+12]=this.cbx_embarque
this.Control[iCurrent+13]=this.st_12
this.Control[iCurrent+14]=this.cbx_recibidor
this.Control[iCurrent+15]=this.st_15
this.Control[iCurrent+16]=this.em_hasta
this.Control[iCurrent+17]=this.st_14
this.Control[iCurrent+18]=this.st_5
this.Control[iCurrent+19]=this.st_9
this.Control[iCurrent+20]=this.st_6
this.Control[iCurrent+21]=this.st_13
this.Control[iCurrent+22]=this.uo_selespecie
this.Control[iCurrent+23]=this.cbx_varirotula
this.Control[iCurrent+24]=this.uo_selcondicion
this.Control[iCurrent+25]=this.st_11
this.Control[iCurrent+26]=this.st_17
this.Control[iCurrent+27]=this.cbx_cantpallet
this.Control[iCurrent+28]=this.uo_selcliente
this.Control[iCurrent+29]=this.uo_selplantas
this.Control[iCurrent+30]=this.uo_selproductor
this.Control[iCurrent+31]=this.uo_seltransportista
this.Control[iCurrent+32]=this.uo_seltipocamion
end on

on w_info_consolidado_general_reci.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_embarques)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.em_fzarpe)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.st_10)
destroy(this.dw_operaciones)
destroy(this.cbx_operacion)
destroy(this.cbx_embarque)
destroy(this.st_12)
destroy(this.cbx_recibidor)
destroy(this.st_15)
destroy(this.em_hasta)
destroy(this.st_14)
destroy(this.st_5)
destroy(this.st_9)
destroy(this.st_6)
destroy(this.st_13)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.uo_selcondicion)
destroy(this.st_11)
destroy(this.st_17)
destroy(this.cbx_cantpallet)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
destroy(this.uo_selproductor)
destroy(this.uo_seltransportista)
destroy(this.uo_seltipocamion)
end on

event open;call super::open;Boolean 	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCondicion.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelTransportista.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelTipoCamion.Codigo) Then lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True,True)
	uo_SelCondicion.Seleccion(True, True)
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, True)
	uo_SelTransportista.Seleccion(True, False)
	uo_SelTipoCamion.Seleccion(True, False)
	
	uo_SelPlantas.Filtra(1)
	uo_SelCliente.Inicia(gi_CodExport)		
	uo_SelProductor.Filtra(-1)

	dw_embarques.GetChild("embq_codigo", idwc_embarque)
	idwc_embarque.SetTransObject(SQLCA)
	idwc_embarque.Retrieve(gi_CodExport, 0)
	dw_embarques.InsertRow(0)
	
	dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
	idwc_operaciones.SetTransObject(SQLCA)
	idwc_operaciones.Retrieve(gi_CodExport)
	dw_operaciones.InsertRow(0)
	
	is_embarque					=	'*'
	is_NomEmbarque				=	""
	ii_operacion						=	-1
	is_Operacion					=	'Todos'
	
	em_fzarpe.text					=	String(Today())	
	em_hasta.text					=	String(Today())
	
	dw_embarques.Enabled		=	False
	cbx_embarque.Enabled		=	False
	cbx_embarque.Checked		=	True
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_consolidado_general_reci
integer x = 3515
integer y = 776
end type

type st_computador from w_para_informes`st_computador within w_info_consolidado_general_reci
end type

type st_usuario from w_para_informes`st_usuario within w_info_consolidado_general_reci
end type

type st_temporada from w_para_informes`st_temporada within w_info_consolidado_general_reci
end type

type p_logo from w_para_informes`p_logo within w_info_consolidado_general_reci
integer width = 347
integer height = 288
end type

type st_titulo from w_para_informes`st_titulo within w_info_consolidado_general_reci
integer width = 2949
string text = "Consolidado General de Embarques por Recibidores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consolidado_general_reci
integer x = 3538
integer y = 1160
integer taborder = 130
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Long	  	ll_Fila
Integer	li_consreci, li_varirotula
String		t_fecha, is_Instructivo
Date 		ld_fechaZarpe

SetPointer(HourGlass!)

istr_info.titulo	= 'CONSOLIDADO GENERAL DE EMBARQUES POR RECIBIDOR'
OpenWithParm(vinf, istr_info)

If cbx_cantpallet.Checked Then
	vinf.dw_1.DataObject = "dw_info_consolidado_gral_reci_cantpallet"
Else
	vinf.dw_1.DataObject = "dw_info_consolidado_gral_reci"
End If	

vinf.dw_1.SetTransObject(sqlca)

If cbx_recibidor.checked Then
	li_consreci = 9
Else
	li_consreci = 0
End If

If cbx_varirotula.Checked Then
	li_varirotula = 1
Else
	li_varirotula = 0
End If

If is_Embarque = 'Z' Then
	is_Instructivo = 'Todos'
Else
	is_Instructivo = is_Embarque
End If


ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, is_Embarque, ii_Operacion, uo_SelEspecie.Codigo, &
									uo_SelPlantas.Codigo, uo_SelProductor.Codigo, 0, li_consreci,&
									Date(em_fzarpe.text), Date(em_hasta.text), uo_SelTransportista.Codigo, uo_SelTipoCamion.Codigo,&
									li_varirotula,uo_SelCondicion.Codigo)
If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.ModIfy("planta.text = '" + uo_SelPlantas.Nombre + "'")
	vinf.dw_1.ModIfy("especie.text = '" + uo_SelEspecie.Nombre + "'")
	vinf.dw_1.ModIfy("operacion.text = '" + is_Operacion + "'")
	vinf.dw_1.ModIfy("transportista.text = '" +  uo_SelTransportista.Nombre + "'")
	vinf.dw_1.ModIfy("tipocamion.text = '" + uo_SelTipoCamion.Nombre + "'")
	
	vinf.dw_1.ModIfy("embarque.text = '" + is_Instructivo + "'")
	vinf.dw_1.ModIfy("nave.text = '" + is_NomEmbarque + "'")
	
	vinf.dw_1.ModIfy("desde.text = '" + em_fzarpe.text+ "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_hasta.text+ "'")
	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If


end event

type pb_salir from w_para_informes`pb_salir within w_info_consolidado_general_reci
integer x = 3538
integer y = 1520
integer taborder = 140
end type

type st_1 from statictext within w_info_consolidado_general_reci
integer x = 256
integer y = 440
integer width = 1426
integer height = 724
boolean bringtotop = true
integer textsize = -10
integer weight = 400
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

type st_2 from statictext within w_info_consolidado_general_reci
integer x = 320
integer y = 504
integer width = 270
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

type dw_embarques from datawindow within w_info_consolidado_general_reci
integer x = 2144
integer y = 740
integer width = 960
integer height = 92
integer taborder = 110
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_embarques_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_Nula

SetNull(ls_Nula)

IF ExistEmbarque(uo_SelCliente.Codigo, data, ls_Columna[]) THEN
	is_Embarque		=	ls_Columna[3]
	IF Integer(ls_Columna[5]) <> ii_Operacion THEN
		MessageBox("Atención", "Embarque corresponde a otra Operación (" + &
						ls_Columna[3] + ")")
		is_Embarque	=	""
		This.SetItem(1, "embq_codigo", ls_Nula)
						
		RETURN 1
	END IF
	
   IF ii_Operacion = 0 THEN
		dw_operaciones.SetItem(1, "oper_codigo", Integer(ls_Columna[5]))
		ii_Operacion = Integer(ls_Columna[3])
	END IF
//ELSE
//	is_Embarque	=	""
//	This.SetItem(1, "embq_codigo", ls_Nula)
//	
//	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_consolidado_general_reci
integer x = 320
integer y = 704
integer width = 270
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_consolidado_general_reci
integer x = 1755
integer y = 768
integer width = 311
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
string text = "Embarque"
boolean focusrectangle = false
end type

type em_fzarpe from editmask within w_info_consolidado_general_reci
integer x = 745
integer y = 1452
integer width = 489
integer height = 92
integer taborder = 120
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
boolean dropdowncalendar = true
end type

type st_7 from statictext within w_info_consolidado_general_reci
integer x = 251
integer y = 1168
integer width = 2949
integer height = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 400
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

type st_8 from statictext within w_info_consolidado_general_reci
integer x = 320
integer y = 884
integer width = 270
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

type st_10 from statictext within w_info_consolidado_general_reci
integer x = 1746
integer y = 584
integer width = 315
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
string text = "Operacion"
boolean focusrectangle = false
end type

type dw_operaciones from datawindow within w_info_consolidado_general_reci
integer x = 2144
integer y = 556
integer width = 974
integer height = 92
integer taborder = 90
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteOperacion(uo_SelCliente.Codigo, Integer(data), ls_Columna[]) THEN
	ii_Operacion	=	Integer(data)
	idwc_embarque.retrieve(uo_SelCliente.Codigo, ii_OPeracion)
	is_Operacion	=	String(ii_Operacion,'###') + ' ' + ls_Columna[4]
	is_NomEmbarque	=	ls_Columna[1] + ' ' +ls_Columna[2]	
END IF
end event

event itemerror;RETURN 1
end event

type cbx_operacion from checkbox within w_info_consolidado_general_reci
integer x = 2149
integer y = 484
integer width = 279
integer height = 72
integer taborder = 80
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
end type

event clicked;IF This.Checked THEN
	ii_Operacion						=	-1
	is_embarque					=	"*"
	is_operacion					=	"Todos"
	is_nomembarque				=  "Todos"
	dw_operaciones.Enabled		=	False
	dw_operaciones.Enabled		=	False
	dw_embarques.Enabled		=	False
	cbx_embarque.Enabled		=	False
	cbx_embarque.Checked		=	True
	dw_operaciones.Reset()
   	dw_operaciones.InsertRow(0)
	dw_embarques.Reset()
  	dw_embarques.InsertRow(0)
ELSE
	
	dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
	idwc_operaciones.SetTransObject(SQLCA)	
	idwc_operaciones.Retrieve(uo_SelCliente.Codigo)
	dw_operaciones.InsertRow(0)	
	
	dw_operaciones.Enabled		=	True
	cbx_embarque.Enabled		=	True
	cbx_embarque.Checked		=	True
	dw_operaciones.SetFocus()
END IF
end event

type cbx_embarque from checkbox within w_info_consolidado_general_reci
integer x = 2158
integer y = 672
integer width = 279
integer height = 72
integer taborder = 100
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
end type

event clicked;IF This.Checked THEN
	is_embarque	=	'*'
	dw_embarques.Enabled	=	False
	dw_embarques.Reset()
	dw_embarques.InsertRow(0)
ELSE
	dw_embarques.Enabled	=	True
	dw_embarques.SetFocus()
END IF
end event

type st_12 from statictext within w_info_consolidado_general_reci
integer x = 325
integer y = 1060
integer width = 297
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
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_recibidor from checkbox within w_info_consolidado_general_reci
integer x = 315
integer y = 1624
integer width = 850
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Recibidores Consolidados "
end type

event clicked;IF This.Checked THEN
	is_embarque	=	'Z'
	dw_embarques.Enabled	=	False
	dw_embarques.Reset()
	dw_embarques.InsertRow(0)
ELSE
	dw_embarques.Enabled	=	True
	dw_embarques.SetFocus()
END IF
end event

type st_15 from statictext within w_info_consolidado_general_reci
integer x = 251
integer y = 1588
integer width = 2949
integer height = 168
integer textsize = -10
integer weight = 400
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

type em_hasta from editmask within w_info_consolidado_general_reci
integer x = 2368
integer y = 1452
integer width = 489
integer height = 92
integer taborder = 130
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
boolean dropdowncalendar = true
end type

type st_14 from statictext within w_info_consolidado_general_reci
integer x = 1975
integer y = 1472
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
string text = "Hasta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_consolidado_general_reci
integer x = 306
integer y = 1280
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
string text = "Transportista"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_consolidado_general_reci
integer x = 1696
integer y = 1296
integer width = 389
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
string text = "Tipo Camión"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_consolidado_general_reci
integer x = 320
integer y = 1472
integer width = 407
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha  Desde"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_consolidado_general_reci
integer x = 251
integer y = 1404
integer width = 2949
integer height = 184
integer textsize = -10
integer weight = 400
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

type uo_selespecie from uo_seleccion_especie within w_info_consolidado_general_reci
event destroy ( )
integer x = 718
integer y = 608
integer height = 180
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_consolidado_general_reci
integer x = 1408
integer y = 1624
integer width = 635
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
string text = "Variedad Rotulada"
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_consolidado_general_reci
integer x = 2144
integer y = 868
integer taborder = 280
boolean bringtotop = true
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

type st_11 from statictext within w_info_consolidado_general_reci
integer x = 1682
integer y = 440
integer width = 1509
integer height = 724
integer textsize = -10
integer weight = 400
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

type st_17 from statictext within w_info_consolidado_general_reci
integer x = 1755
integer y = 940
integer width = 311
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

type cbx_cantpallet from checkbox within w_info_consolidado_general_reci
integer x = 2368
integer y = 1624
integer width = 800
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
string text = "Incluye Cantidad Pallet"
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_consolidado_general_reci
event destroy ( )
integer x = 718
integer y = 492
integer height = 92
integer taborder = 70
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_consolidado_general_reci
event destroy ( )
integer x = 718
integer y = 784
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_consolidado_general_reci
event destroy ( )
integer x = 718
integer y = 968
integer taborder = 290
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_seltransportista from uo_seleccion_transportista within w_info_consolidado_general_reci
event destroy ( )
integer x = 718
integer y = 1192
integer taborder = 130
boolean bringtotop = true
end type

on uo_seltransportista.destroy
call uo_seleccion_transportista::destroy
end on

type uo_seltipocamion from uo_seleccion_tipocamion within w_info_consolidado_general_reci
event destroy ( )
integer x = 2144
integer y = 1192
integer taborder = 140
boolean bringtotop = true
end type

on uo_seltipocamion.destroy
call uo_seleccion_tipocamion::destroy
end on

