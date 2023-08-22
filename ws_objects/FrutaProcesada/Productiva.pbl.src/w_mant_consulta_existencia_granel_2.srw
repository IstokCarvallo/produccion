$PBExportHeader$w_mant_consulta_existencia_granel_2.srw
forward
global type w_mant_consulta_existencia_granel_2 from w_mant_tabla
end type
type dw_3 from datawindow within w_mant_consulta_existencia_granel_2
end type
type st_6 from statictext within w_mant_consulta_existencia_granel_2
end type
type st_1 from statictext within w_mant_consulta_existencia_granel_2
end type
type st_2 from statictext within w_mant_consulta_existencia_granel_2
end type
type em_desde from editmask within w_mant_consulta_existencia_granel_2
end type
type em_hasta from editmask within w_mant_consulta_existencia_granel_2
end type
type st_8 from statictext within w_mant_consulta_existencia_granel_2
end type
type cbx_diferencias from checkbox within w_mant_consulta_existencia_granel_2
end type
type st_3 from statictext within w_mant_consulta_existencia_granel_2
end type
type cbx_item from checkbox within w_mant_consulta_existencia_granel_2
end type
type em_item from editmask within w_mant_consulta_existencia_granel_2
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_consulta_existencia_granel_2
end type
type uo_selproductor from uo_seleccion_productor within w_mant_consulta_existencia_granel_2
end type
type dw_2 from datawindow within w_mant_consulta_existencia_granel_2
end type
end forward

global type w_mant_consulta_existencia_granel_2 from w_mant_tabla
integer width = 5111
integer height = 2572
string title = "GUIAS FALTANTES EN EXISTENCIA"
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
dw_3 dw_3
st_6 st_6
st_1 st_1
st_2 st_2
em_desde em_desde
em_hasta em_hasta
st_8 st_8
cbx_diferencias cbx_diferencias
st_3 st_3
cbx_item cbx_item
em_item em_item
uo_selcliente uo_selcliente
uo_selproductor uo_selproductor
dw_2 dw_2
end type
global w_mant_consulta_existencia_granel_2 w_mant_consulta_existencia_granel_2

type variables
String	is_rut
Boolean	ib_ConectadoExistencia
Long		ii_productor
Integer	il_packing

Datawindowchild idwc_cliente, idwc_productor

Transaction			sqlexi
end variables

forward prototypes
public function boolean conexionexistencia (integer ai_conexion)
end prototypes

public function boolean conexionexistencia (integer ai_conexion);String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

If ib_ConectadoExistencia Then
	ib_ConectadoExistencia	=	False
	DISCONNECT USING sqlexi;
End If

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT	cone_nomodb,cone_nomser,cone_nombas,
			cone_nodbms,cone_nomusu,cone_passwo  
 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
		:ls_nodbms,:ls_Usuario,:ls_Password
 FROM dbo.prodconectividad   
WHERE cone_codigo = :ai_conexion;

sqlexi.ServerName		=	ls_nomser
sqlexi.DataBase			=	ls_nombas
sqlexi.Dbms				= 	ls_nodbms
sqlexi.DbParm			=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + ";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
CONNECT USING sqlexi;

If sqlexi.SQLCode = 0 Then ib_ConectadoExistencia	=	True

Return ib_ConectadoExistencia

end function

on w_mant_consulta_existencia_granel_2.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.st_6=create st_6
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_8=create st_8
this.cbx_diferencias=create cbx_diferencias
this.st_3=create st_3
this.cbx_item=create cbx_item
this.em_item=create em_item
this.uo_selcliente=create uo_selcliente
this.uo_selproductor=create uo_selproductor
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_8
this.Control[iCurrent+8]=this.cbx_diferencias
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.cbx_item
this.Control[iCurrent+11]=this.em_item
this.Control[iCurrent+12]=this.uo_selcliente
this.Control[iCurrent+13]=this.uo_selproductor
this.Control[iCurrent+14]=this.dw_2
end on

on w_mant_consulta_existencia_granel_2.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.st_6)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.cbx_diferencias)
destroy(this.st_3)
destroy(this.cbx_item)
destroy(this.em_item)
destroy(this.uo_selcliente)
destroy(this.uo_selproductor)
destroy(this.dw_2)
end on

event ue_validaborrar;call super::ue_validaborrar;IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

event closequery;//
end event

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelCliente.Inicia(81)
	
	dw_2.SetTransObject(sqlca)
	
	em_desde.Text			=	String(RelativeDate(Today(), -30))
	em_hasta.Text			=	String(Today())
	
	istr_mant.Argumento[13] = "-1"
End If
end event

event ue_recuperadatos;Long		ll_fila, ll_guia, ll_new, ll_numero, ll_nueva, ll_fila_find, ll_Cont, ll_fffff
Integer	li_coneccion, li_coneccion2
String		ls_nomodb
Date		ld_fecha

Open(w_mensaje)
w_mensaje.st_1.text = "Carga en Curso"
		
SELECT 	cone_codigo
	INTO   	:li_coneccion
 	FROM dbo.clientesprod
 	WHERE clie_codigo = :uo_SelCliente.Codigo;

sqlexi	=	CREATE Transaction

uo_existencia	luo_existencia
luo_existencia	=	Create uo_existencia

If Conexionexistencia(li_coneccion) Then
	FOR ll_fila = 1 TO dw_1.RowCount()
		dw_3.SetTransObject(sqlexi)
		
		ll_guia 		= dw_1.Object.mfge_guisii[ll_fila]
		ll_numero 	= dw_1.Object.mfge_numero[ll_fila]
		ld_fecha		= dw_1.Object.mfge_fecmov[ll_fila]

		If ll_numero = 95 Then ll_fffff=1
		
		dw_3.Retrieve(ll_guia, ll_numero, ld_fecha, istr_mant.Argumento[13])
		
		If dw_3.RowCount() > 0 Then
			FOR ll_nueva = 1 TO dw_3.RowCount()
				ll_fila_find = dw_1.Find("mfge_numero = " + String(dw_3.Object.mden_docrel[ll_nueva])+ " and " + &
									"enva_tipoen = " + String(dw_3.Object.enva_tipoen[ll_nueva])+ " and " + &						
									"enva_codigo = " + String(dw_3.Object.enva_codigo[ll_nueva])+ " and " + &
									"mfge_guisii = " + String(dw_3.Object.mden_gdprod[ll_nueva]),ll_fila, dw_1.RowCount())
									
			
				If ll_fila_find >0 And dw_1.Object.bins_ctacte[ll_fila] <> 1 Then	 
					
					dw_1.Object.bodega1[ll_fila_find] 	  	= dw_3.Object.bode_codigo[ll_nueva]
					dw_1.Object.boddest1[ll_fila_find]		= dw_3.Object.mden_bodest[ll_nueva]
					dw_1.Object.existencia1[ll_fila_find] 	= dw_3.Object.mden_numero[ll_nueva]
					dw_1.Object.fecha1[ll_fila_find] 		= dw_3.Object.mden_fecmov[ll_nueva]
					dw_1.Object.item1[ll_fila_find]  		= dw_3.Object.item_codigo[ll_nueva]
					dw_1.Object.cantidad1[ll_fila_find] 	= dw_3.Object.mdde_cantid[ll_nueva]
					dw_1.Object.interplanta[ll_fila_find] 	= dw_3.Object.tpmv_codigo[ll_nueva]
								
					luo_existencia.bodega_administradora(dw_1.Object.boddest1[ll_fila_find],True, sqlexi)
					
					If luo_existencia.bodeAdmi <> dw_1.Object.bodega1[ll_fila_find] Then
						il_packing = dw_1.Object.boddest1[ll_fila_find]
						
						SELECT isnull(plde_conpro,0)
						INTO :li_coneccion 
						FROM dbo.plantadesp
						WHERE plde_codigo = :il_packing;
						
						If li_coneccion = 0 Then li_coneccion = 95
					Else
						li_coneccion = 96
					End If
					
					SELECT cone_nomodb
					 INTO :ls_nomodb
					 FROM dbo.prodconectividad   
					WHERE cone_codigo = :li_coneccion;
					
					dw_1.Object.conexion[ll_fila_find] = li_coneccion
					dw_1.Object.base[ll_fila_find] 		= ls_nomodb					
				End If	
			Next
		End If	
	Next
	
	dw_1.SetSort("conexion asc, mfge_numero desc")
	dw_1.Sort( )
End If

FOR ll_Fila = 1 TO dw_1.RowCount()
	li_coneccion = dw_1.Object.conexion[ll_fila] 
	If li_coneccion <> li_coneccion2 AND li_coneccion <> 0 Then
		DISCONNECT USING sqlexi;
		If Conexionexistencia(li_coneccion) Then
			ll_guia 		= dw_1.Object.mfge_guisii[ll_Fila]
			ll_numero 	= dw_1.Object.mfge_numero[ll_Fila]
			ld_fecha		= dw_1.Object.mfge_fecmov[ll_fila]
			
			dw_2.SetTransObject(sqlexi)
			dw_2.Retrieve(ll_numero,ll_guia,ld_fecha,istr_mant.argumento[13])
				
			If dw_2.RowCount() > 0 Then
				FOR ll_nueva = 1 TO dw_2.RowCount()

					If  dw_2.Object.mden_docrel[ll_nueva] = 2724 Then ll_fffff=1
						
					ll_fila_find = dw_1.Find("mfge_numero = " + String(dw_2.Object.mden_docrel[ll_nueva])+ " and " + &
													"enva_tipoen = " + String(dw_2.Object.enva_tipoen[ll_nueva])+ " and " + &
													"enva_codigo = " + String(dw_2.Object.enva_codigo[ll_nueva])+ " and " + &
													"mfge_guisii = " + String(dw_2.Object.mden_gdprod[ll_nueva]),ll_fila, dw_1.RowCount())
			
					If ll_fila_find > 0 AND dw_1.Object.bins_ctacte[ll_fila] <> 1 Then	   
						dw_1.Object.bodega2[ll_fila_find] 	  	= dw_2.Object.bode_codigo[ll_nueva]
						dw_1.Object.boddest2[ll_fila_find] 	  	= dw_2.Object.mden_bodest[ll_nueva]
						dw_1.Object.existencia2[ll_fila_find] 	= dw_2.Object.mden_numero[ll_nueva]
						dw_1.Object.fecha2[ll_fila_find] 		= dw_2.Object.mden_fecmov[ll_nueva]
						dw_1.Object.item2[ll_fila_find]  		= dw_2.Object.item_codigo[ll_nueva]
						dw_1.Object.cantidad2[ll_fila_find] 	= dw_2.Object.mdde_cantid[ll_nueva]
						dw_1.Object.interplanta[ll_fila_find] 	= dw_2.Object.tpmv_codigo[ll_nueva]
					End If	
				Next
			End If
		End If
		li_coneccion2 = li_coneccion
	ElseIf li_coneccion <> 0 Then
		
		ll_guia 		= dw_1.Object.mfge_guisii[ll_Fila]
		ll_numero 	= dw_1.Object.mfge_numero[ll_Fila]
		ld_fecha		= dw_1.Object.mfge_fecmov[ll_fila]
		
		dw_2.SetTransObject(sqlexi)
		ll_Cont = dw_2.Retrieve(ll_numero,ll_guia,ld_fecha,istr_mant.argumento[13])
		If dw_2.RowCount() > 0 Then
			FOR ll_nueva = 1 TO dw_2.RowCount()
				
				ll_fila_find = dw_1.Find("mfge_numero = " + String(dw_2.Object.mden_docrel[ll_nueva])+ " and " + &
												"enva_tipoen = " + String(dw_2.Object.enva_tipoen[ll_nueva])+ " and " + &
												"enva_codigo = " + String(dw_2.Object.enva_codigo[ll_nueva])+ " and " + &
												"mfge_guisii = " + String(dw_2.Object.mden_gdprod[ll_nueva]),ll_fila, dw_1.RowCount())
		
				If ll_fila_find > 0 AND dw_1.Object.bins_ctacte[ll_fila] <> 1 Then	   
					dw_1.Object.bodega2[ll_fila_find] 	  	= dw_2.Object.bode_codigo[ll_nueva]
					dw_1.Object.boddest2[ll_fila_find] 	  	= dw_2.Object.mden_bodest[ll_nueva]
					dw_1.Object.existencia2[ll_fila_find] 	= dw_2.Object.mden_numero[ll_nueva]
					dw_1.Object.fecha2[ll_fila_find] 		= dw_2.Object.mden_fecmov[ll_nueva]
					dw_1.Object.item2[ll_fila_find]  		= dw_2.Object.item_codigo[ll_nueva]
					dw_1.Object.cantidad2[ll_fila_find] 	= dw_2.Object.mdde_cantid[ll_nueva]
					dw_1.Object.interplanta[ll_fila_find] 	= dw_2.Object.tpmv_codigo[ll_nueva]
				End If	
			Next
		End If
	End If
Next

dw_1.SetSort("mfge_numero desc")
dw_1.Sort( )

If cbx_dIferencias.Checked Then
	dw_1.SetFilter("bins_ctacte <> 1 and (cantidad1 = 0 OR cantidad2 = 0)")
	dw_1.Filter()
End If

Close(w_mensaje)	

	
end event

event close;//
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "MOVIMIENTO DE ENVASES"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_llena_mov_envases"
vinf.dw_1.SetTransObject(sqlca)
fila = dw_1.ShareData(vinf.dw_1)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
	vinf.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
End If

SetPointer(Arrow!)
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_consulta_existencia_granel_2
integer x = 27
integer y = 332
integer width = 4462
integer height = 2232
integer taborder = 20
string dataobject = "dw_mues_llena_con_guias_2"
boolean hscrollbar = true
boolean livescroll = false
end type

event dw_1::clicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::getfocus;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF dwo.Name = "empr_rutemp" THEN
		IF is_rut <> "" THEN
			This.SetItem(1, "empr_rutemp", String(Double(Mid(is_rut, 1, 9)), "#########") + Mid(is_rut, 10))
		END IF
	ELSE
		This.SetItem(1, "empr_rutemp", is_rut)
	END IF
END IF
end event

event dw_1::itemerror;RETURN 1
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_consulta_existencia_granel_2
integer x = 27
integer y = 0
integer width = 4457
integer height = 316
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_consulta_existencia_granel_2
boolean visible = false
integer x = 5074
integer y = 264
integer taborder = 0
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_consulta_existencia_granel_2
boolean visible = false
integer x = 5070
integer y = 560
integer taborder = 0
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_consulta_existencia_granel_2
boolean visible = false
integer x = 5070
integer y = 740
integer taborder = 0
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_consulta_existencia_granel_2
boolean visible = false
integer x = 5070
integer y = 920
integer taborder = 0
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_consulta_existencia_granel_2
integer x = 4590
integer y = 492
integer taborder = 30
boolean enabled = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

event pb_grabar::clicked;If dw_1.Retrieve(uo_SelCliente.Codigo, Date(em_Desde.Text), Date(em_Hasta.Text), uo_SelProductor.Codigo, istr_mant.Argumento[13]) = 0 Then
	Close(Parent)
	Return 1
Else
	Parent.TriggerEvent("ue_recuperadatos")
End If



end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_consulta_existencia_granel_2
integer x = 4585
integer y = 792
integer taborder = 0
boolean enabled = true
long backcolor = 553648127
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_consulta_existencia_granel_2
integer x = 4594
integer y = 1584
integer taborder = 40
long backcolor = 553648127
end type

type dw_3 from datawindow within w_mant_consulta_existencia_granel_2
boolean visible = false
integer x = 1426
integer y = 1100
integer width = 686
integer height = 400
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_guia_existencia_2"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_mant_consulta_existencia_granel_2
integer x = 137
integer y = 64
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

type st_1 from statictext within w_mant_consulta_existencia_granel_2
integer x = 137
integer y = 160
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
string text = "Desde"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_consulta_existencia_granel_2
integer x = 946
integer y = 160
integer width = 178
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

type em_desde from editmask within w_mant_consulta_existencia_granel_2
integer x = 407
integer y = 148
integer width = 489
integer height = 92
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;istr_mant.argumento[3]	=	This.Text
end event

type em_hasta from editmask within w_mant_consulta_existencia_granel_2
integer x = 1125
integer y = 148
integer width = 489
integer height = 92
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
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;istr_mant.argumento[13]	=	This.Text
end event

type st_8 from statictext within w_mant_consulta_existencia_granel_2
integer x = 1719
integer y = 160
integer width = 302
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

type cbx_diferencias from checkbox within w_mant_consulta_existencia_granel_2
integer x = 3319
integer y = 56
integer width = 608
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
string text = "Solo Diferencias"
end type

event clicked;IF THIS.Checked THEN
	dw_1.SetFilter("bins_ctacte <> 1 and (cantidad1 = 0 OR cantidad2 = 0)")
	dw_1.Filter()
ELSE	
	dw_1.SetFilter("")
	dw_1.Filter()
END IF	
end event

type st_3 from statictext within w_mant_consulta_existencia_granel_2
integer x = 3008
integer y = 160
integer width = 302
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
string text = "Item"
boolean focusrectangle = false
end type

type cbx_item from checkbox within w_mant_consulta_existencia_granel_2
integer x = 4055
integer y = 160
integer width = 315
integer height = 64
integer taborder = 30
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
	em_item.Enabled				=	False
	em_item.Text					=	''
	istr_mant.Argumento[13]	=	'-1'
ELSE
	em_item.Enabled				=	True
	em_item.Text					=	''
	istr_mant.argumento[13]	=	''
END IF
end event

type em_item from editmask within w_mant_consulta_existencia_granel_2
integer x = 3323
integer y = 148
integer width = 690
integer height = 92
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;istr_mant.argumento[13]	=	This.Text
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_consulta_existencia_granel_2
integer x = 407
integer y = 52
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_mant_consulta_existencia_granel_2
event destroy ( )
integer x = 2034
integer y = 64
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type dw_2 from datawindow within w_mant_consulta_existencia_granel_2
boolean visible = false
integer x = 649
integer y = 1092
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_guia_existencia_3"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

