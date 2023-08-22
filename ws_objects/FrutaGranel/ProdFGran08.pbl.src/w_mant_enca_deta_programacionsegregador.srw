$PBExportHeader$w_mant_enca_deta_programacionsegregador.srw
forward
global type w_mant_enca_deta_programacionsegregador from w_mant_encab_deta_csd
end type
type cb_copia from commandbutton within w_mant_enca_deta_programacionsegregador
end type
type dw_4 from uo_dw within w_mant_enca_deta_programacionsegregador
end type
type dw_3 from uo_dw within w_mant_enca_deta_programacionsegregador
end type
type dw_5 from uo_dw within w_mant_enca_deta_programacionsegregador
end type
end forward

global type w_mant_enca_deta_programacionsegregador from w_mant_encab_deta_csd
boolean visible = false
integer width = 3858
integer height = 2112
string title = "Programacion Segregador"
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
cb_copia cb_copia
dw_4 dw_4
dw_3 dw_3
dw_5 dw_5
end type
global w_mant_enca_deta_programacionsegregador w_mant_enca_deta_programacionsegregador

type variables
DataWindowChild				idwc_Salida

uo_Especie						iuo_Especies
uo_programasegregador		iuo_Segregador
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor, integer ai_fila)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaingreso (string columna)
end prototypes

event ue_validapassword();str_mant	lstr_mant

IF Not IsNull(gstr_paramplanta.passpack) OR gstr_paramplanta.passpack <> '' THEN
	lstr_mant.Argumento[1]	=	"Granel"
	lstr_mant.Argumento[2]	=	gstr_paramplanta.passpack
	
	OpenWithParm(w_password, lstr_mant)
	
	lstr_mant	=	Message.PowerObjectParm
	
	IF lstr_mant.Respuesta = 0 THEN Close(This)
END IF
end event

public function boolean duplicado (string as_columna, string as_valor, integer ai_fila);Integer	li_salidas, li_busq
String		ls_embalajes, ls_calibres

li_salidas 		= 	dw_1.Object.lisa_codigo[ai_fila]
ls_embalajes	=	dw_1.Object.emba_codigo[ai_fila]
ls_calibres		=	dw_1.Object.prde_calibr[ai_fila]

Choose Case as_columna
	Case "lisa_codigo"
		li_salidas 		=	Integer(as_valor)
		
	Case "emba_codigo"
		ls_embalajes	=	as_valor
		
	Case "prde_calibr"
		ls_calibres 		=	as_valor
		
End Choose

li_busq	=	dw_1.Find(	"lisa_codigo	= "	+ String(li_salidas)	+ " AND " + &
								"emba_codigo 	= '" 	+ ls_embalajes			+ "' AND " + &
								"prde_calibr 	= '" 	+ ls_calibres 			+	"'", 1, dw_1.RowCount())
If li_busq > 0 And li_busq <> ai_Fila Then
	MessageBox("Protección de Duplicidad", "La Programación ingresada ya existe. ~r~n"+&
					 "Salida 	:" 		+ String(li_salidas, '00') 	+ "~r~n" + &
					 "Embalaje	:" 	+ ls_embalajes				+ "~r~n" + &
					 "Calibre	:" 		+ ls_calibres)
	 Return True
Else
	Return False
End If
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.espe_codigo.Protect					=	0
  	dw_2.Object.pren_fechas.Protect					=	0
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
  	dw_2.Object.pren_fechas.BackGround.Color		=	RGB(255,255,255)
ELSE
	dw_2.Object.espe_codigo.Protect					=	1
  	dw_2.Object.pren_fechas.Protect					=	1
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
  	dw_2.Object.pren_fechas.BackGround.Color		=	RGB(192,192,192)
END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

dw_2.AcceptText()

If columna <> "espe_codigo" And &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_estado = False
End If
	
If columna <> "pren_fechas" And &
	(IsNull(dw_2.Object.pren_fechas[1])) THEN
	lb_estado = False
End If

dw_4.Retrieve(dw_2.Object.espe_codigo[1], dw_2.Object.pren_fechas[1])

pb_ins_det.Enabled	= lb_estado
cb_Copia.Enabled 		= lb_estado
end subroutine

on w_mant_enca_deta_programacionsegregador.create
int iCurrent
call super::create
this.cb_copia=create cb_copia
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_copia
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_3
this.Control[iCurrent+4]=this.dw_5
end on

on w_mant_enca_deta_programacionsegregador.destroy
call super::destroy
destroy(this.cb_copia)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_5)
end on

event open;call super::open;iuo_Especies		=	Create uo_Especie
iuo_Segregador	=	Create uo_programasegregador

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

dw_1.GetChild('lisa_codigo', idwc_Salida)
idwc_Salida.SetTransObject(Sqlca)
idwc_Salida.Retrieve(gstr_ParamPlanta.CodigoPlanta, 99)

buscar	= "Código Salida:Nlisa_codigo"
ordenar	= "Código Salida:lisa_codigo"

This.TriggerEvent("ue_validapassword")
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_e, respuesta

Do
	ll_fila_e	=	dw_2.Retrieve(iuo_Segregador.Especie, iuo_Segregador.Fecha)
										  
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)		
	Else
		Do						
			If dw_1.Retrieve(iuo_Segregador.Especie, iuo_Segregador.Fecha) = -1 OR & 
				dw_3.Retrieve(gstr_ParamPlanta.CodigoPlanta, 99) = -1 OR &
				dw_4.Retrieve(iuo_Segregador.Especie, iuo_Segregador.Fecha) = -1 OR &
				dw_5.Retrieve(iuo_Segregador.Especie, -1, -1) = -1 Then
	  
				Respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.",  Information!, RetryCancel!)
			Else
				
				dw_1.GetChild('lisa_codigo', idwc_Salida)
				idwc_Salida.SetTransObject(Sqlca)
				idwc_Salida.Retrieve(gstr_ParamPlanta.CodigoPlanta, 99)
				
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				pb_eli_det.Enabled	=	True
				HabilitaEncab(False)
			End If
		Loop While respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
Loop While respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer	li_Linea, li_Salida
Long		ll_Fila, ll_New, ll_Null
String		ls_Embalaje, ls_Calibre

SetNull(li_Linea)
SetNull(li_Salida)
SetNull(ls_Embalaje)
SetNull(ls_Calibre)
SetNull(ll_Null)

For ll_Fila = 1 TO dw_3.RowCount()
	If dw_3.IsSelected(ll_Fila) Then
		li_salida	= 	dw_3.Object.lisa_codigo[ll_Fila]
		li_Linea	=	dw_3.Object.line_codigo[ll_Fila]
	End If
Next

If IsNull(li_Salida) Then
	MessageBox("Error", "No ha seleccionado una Salida para programar", StopSign!, OK!)
	Return
End If

For ll_Fila = 1 TO dw_4.RowCount()
	If dw_4.IsSelected(ll_Fila) Then
		ls_Embalaje	= 	dw_4.Object.emba_codigo[ll_Fila]
	End If
Next

If IsNull(ls_Embalaje) Then
	MessageBox("Error", "No ha seleccionado un Embalaje para programar", StopSign!, OK!)
	Return
End If

For ll_Fila = 1 TO dw_5.RowCount()
	If dw_5.IsSelected(ll_Fila) Then
		ls_Calibre	= 	dw_5.Object.caen_calibr[ll_Fila]
	End If
Next

If IsNull(ls_Calibre) Then
	MessageBox("Error", "No ha seleccionado un Calibre para programar", StopSign!, OK!)
	Return
End If

ll_New	=	dw_1.InsertRow(0)

dw_1.Object.line_codigo[ll_New]		=	li_Linea
dw_1.Object.lisa_codigo[ll_New]		=	li_Salida
dw_1.Object.emba_codigo[ll_New]		=	ls_Embalaje
dw_1.Object.prde_calibr[ll_New]		=	ls_Calibre

If Duplicado('lisa_codigo', String(li_Salida), ll_New) Then
	dw_1.DeleteRow(ll_New)
End If

dw_3.SelectRow(0, False)
dw_4.SelectRow(0, False)
dw_5.SelectRow(0, False)

pb_grabar.enabled	=	True
pb_imprimir.enabled	=	True
pb_eliminar.enabled	=	True
end event

event ue_nuevo;call super::ue_nuevo;dw_3.Reset()
dw_4.Reset()
dw_5.Reset()

dw_3.Retrieve(gstr_ParamPlanta.CodigoPlanta, 99)
HabilitaEncab(True)

dw_2.Object.pren_fechas[1]	=	Date(String(Today(), 'dd/mm/yyyy'))

end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_filas

SetPointer(HourGlass!)

w_main.SetMicroHelp("Validando la eliminación de detalle...")
FOR li_filas	=	1 TO dw_1.RowCount()
	IF dw_1.IsSelected(li_filas) THEN
		IF dw_1.DeleteRow(li_filas) = 1 THEN
			w_main.SetMicroHelp("Borrando Registro...")
			li_filas = li_filas - 1
			SetPointer(Arrow!)
		ELSE
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	END IF
NEXT

dw_1.SelectRow(0, False)

 IF dw_1.RowCount() = 0 THEN 
	pb_eli_det.Enabled = False
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "INFORME DE PROGRAMACION DE SALIDAS SEGREGADOR"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_programacionsegregador"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(iuo_Segregador.Especie, iuo_Segregador.Fecha)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;Long	ll_Filas, ll_Secuencia

ll_Secuencia	=	iuo_Segregador.Maximo(iuo_Especies.Codigo, dw_2.Object.pren_fechas[1], Sqlca)

Message.DoubleParm = 0

For ll_Filas = 1 To dw_1.RowCount()
	If	dw_1.GetItemStatus(ll_Filas, 0, Primary!) = NewModified! Or &
		dw_1.GetItemStatus(ll_Filas, 0, Primary!) = DataModified! Then
			dw_1.Object.prde_numero[ll_Filas] = ll_Secuencia
			dw_1.Object.espe_codigo[ll_Filas] = iuo_Especies.Codigo
			dw_1.Object.pren_fechas[ll_Filas] = dw_2.Object.pren_fechas[1]
			dw_1.Object.plde_codigo[ll_Filas] = gstr_ParamPlanta.CodigoPlanta
			ll_Secuencia++
	Else
		If	dw_1.GetItemStatus(ll_Filas, 0, Primary!) = New! Then
			dw_1.DeleteRow(ll_Filas)
		End If
	End If
	
	If IsNull(dw_1.Object.prde_lineas[ll_Filas]) Or dw_1.Object.prde_lineas[ll_Filas] = 0 Then
		MessageBox('Atención', 'Debe ingresar la linea para todos los regsitros.')
		Message.DoubleParm = -1 
		Return
	End If
Next
end event

event ue_seleccion;call super::ue_seleccion;str_Busqueda	lstr_Busq

lstr_Busq	.Argum[1] = '-1'

OpenWithParm(w_busc_programa_segregador, lstr_busq)

lstr_busq	= Message.PowerObjectParm
If UpperBound(lstr_busq.Argum) > 1 Then
	If lstr_busq.argum[1] <> "" Then
		If iuo_Segregador.Existe(Integer(lstr_busq.argum[1]), Date(lstr_Busq.Argum[2]), False, Sqlca) Then TriggerEvent("ue_recuperadatos")
		dw_1.SetFocus()
	End If
End If
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

dw_1.width	=	This.WorkSpaceWidth() - dw_3.Width - 400
maximo		=	dw_1.width

dw_2.x	= 37 
dw_2.y	= 37
If 37 + Round((maximo - dw_2.width) / 2, 0) > 37 Then	dw_2.x	= 37 + Round((maximo - dw_2.width) / 2, 0)

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

dw_3.x 					=	dw_1.width + dw_1.x + 10
dw_4.x 					=	dw_1.width + dw_1.x + 10
dw_5.x 					=	dw_1.width + dw_1.x + dw_4.width + 10

dw_3.y 					=	dw_1.y
dw_3.Height				=	Round((dw_1.Height / 2), 0)

dw_4.y 					=	dw_1.y + dw_3.Height + 5
dw_4.Height				=	dw_3.Height * 1

dw_5.y 					=	dw_1.y + dw_3.Height + 5
dw_5.Height				=	dw_3.Height * 1

cb_copia.y				= pb_eli_det.y + pb_eli_det.height + 10
cb_copia.x				= pb_eli_det.x


end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_mant_enca_deta_programacionsegregador
integer x = 27
integer y = 504
integer width = 2158
integer height = 1424
string title = "Detalle de Programación de Salidas"
string dataobject = "dw_mant_mues_programacionsegregadordeta"
boolean hscrollbar = false
boolean resizable = true
end type

event dw_1::clicked;IF row > 0 THEN THIS.SelectRow(Row, NOT THIS.IsSelected(Row))

end event

event dw_1::getfocus;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::buttonclicked;call super::buttonclicked;Integer	li_Nula
String	ls_Columna, ls_Nula
Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha
str_busqueda lstr_busq

ls_Columna = dwo.Name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "b_tpem"
			
		lstr_busq.argum[1]	=	istr_mant.Argumento[3]
		lstr_busq.argum[2]	=	This.Object.emba_codigo[row]
		
		OpenWithParm(w_busc_tipopallets, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF UpperBound(lstr_busq.argum) > 3 THEN
			IF lstr_busq.argum[3] <> "" THEN
				This.Object.tpem_codigo[row]	=	lstr_busq.argum[3]
			END IF
		ELSE
			Return -1
		END IF
		Return 1

	
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return -1
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_mant_enca_deta_programacionsegregador
integer x = 27
integer y = 24
integer width = 2683
integer height = 472
string dataobject = "dw_mant_programasegregadorenca"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Nula
String		ls_Columna

ls_Columna = dwo.Name

SetNull(li_Nula)

Choose Case ls_Columna			
	Case 'espe_codigo'
		If Not iuo_Especies.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(Row, ls_Columna, li_Nula)
			Return 1
		Else
			IF IsDate(String(This.Object.pren_fechas[Row])) THEN
				If iuo_Segregador.Existe(Integer(Data), This.Object.pren_fechas[Row], False, Sqlca) Then
					Parent.TriggerEvent('ue_recuperadatos')
				End If
			END IF
		End If
		
	Case 'pren_fechas'
		If iuo_Segregador.Existe(iuo_Especies.Codigo, Date(Data), False, Sqlca) Then
			Parent.TriggerEvent('ue_recuperadatos')
		End If
End Choose

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_mant_enca_deta_programacionsegregador
integer x = 3515
integer y = 392
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_mant_enca_deta_programacionsegregador
integer x = 3520
integer y = 572
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_mant_enca_deta_programacionsegregador
integer x = 3520
integer y = 752
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_mant_enca_deta_programacionsegregador
integer x = 3520
integer y = 932
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_mant_enca_deta_programacionsegregador
integer x = 3520
integer y = 1112
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_mant_enca_deta_programacionsegregador
integer x = 3520
integer y = 1480
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_mant_enca_deta_programacionsegregador
integer x = 3520
integer y = 1796
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_mant_enca_deta_programacionsegregador
integer x = 3515
integer y = 212
end type

type cb_copia from commandbutton within w_mant_enca_deta_programacionsegregador
integer x = 3456
integer y = 16
integer width = 233
integer height = 112
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Copia"
end type

event clicked;DataStore		lds_detalle
str_Busqueda	lstr_Busq
Integer			li_cliente, li_tipo
Long				ll_planta, ll_numero, ll_programa, ll_filas
Boolean			lb_linea1, lb_linea2

lds_detalle					=	Create DataSTore
lds_detalle.DataObject	=	dw_1.DataObject
lds_detalle.SetTransObject(sqlca)

dw_1.Reset()
lds_detalle.Reset()

lstr_Busq.Argum[1] = String(iuo_Especies.Codigo)

OpenWithParm(w_busc_programa_segregador, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) > 1 THEN
	IF lstr_busq.argum[1] <> "" THEN
		If lds_detalle.Retrieve(Integer(lstr_Busq.Argum[1]), Date(lstr_Busq.Argum[2])) = 0 Then
			MessageBox('Atencion', 'No se pudo efectuar copia de fecha anterior.', Information!, OK!)
		Else
			lds_detalle.RowsCopy(1, lds_detalle.RowCount(), Primary!, dw_1, dw_1.RowCount() + 1, Primary!)
		End If
	END IF
END IF

pb_grabar.enabled	=	True
pb_imprimir.enabled	=	True
pb_eliminar.enabled	=	True

Destroy lds_Detalle
end event

type dw_4 from uo_dw within w_mant_enca_deta_programacionsegregador
integer x = 2208
integer y = 1264
integer width = 576
integer height = 664
integer taborder = 20
boolean titlebar = true
string title = "Embalajes"
string dataobject = "dw_mues_embalajes_segregador"
boolean livescroll = true
end type

event clicked;call super::clicked;If Row > 0 Then
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	
	If dw_5.Retrieve(iuo_Especies.Codigo, This.Object.enva_tipoen[Row], This.Object.enva_codigo[Row]) = 0 Then
		MessageBox('Atencion', 'Este embalaje no posee calibres para especie Seleccionada.', Information!, OK!)
	End If	
End If

Return 0
end event

type dw_3 from uo_dw within w_mant_enca_deta_programacionsegregador
integer x = 2208
integer y = 504
integer width = 1111
integer height = 752
integer taborder = 11
boolean titlebar = true
string title = "Salidas"
string dataobject = "dw_mues_salidalineapacking_salidas"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event ue_nomover;call super::ue_nomover;uint wParam, lParam

wParam = Message.WordParm

Choose Case wParam
	Case 61456, 61458
	     Message.Processed = True
	     Message.ReturnValue = 0

End Choose
end event

type dw_5 from uo_dw within w_mant_enca_deta_programacionsegregador
integer x = 2784
integer y = 1264
integer width = 526
integer height = 664
integer taborder = 11
boolean titlebar = true
string title = "Calibres"
string dataobject = "dw_mues_calibresenvase_segregador"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

