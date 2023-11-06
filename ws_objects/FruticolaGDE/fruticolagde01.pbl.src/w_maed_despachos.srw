$PBExportHeader$w_maed_despachos.srw
forward
global type w_maed_despachos from w_mant_encab_deta
end type
type dw_3 from uo_dw within w_maed_despachos
end type
type pb_guia from picturebutton within w_maed_despachos
end type
type dw_4 from uo_dw within w_maed_despachos
end type
end forward

global type w_maed_despachos from w_mant_encab_deta
integer width = 3570
integer height = 2288
string title = "DESPACHO DE BINS"
string menuname = ""
event ue_imprimir ( )
event ue_imprimir2 ( )
dw_3 dw_3
pb_guia pb_guia
dw_4 dw_4
end type
global w_maed_despachos w_maed_despachos

type variables
uo_despachobins	iuo_Despachos
uo_transportista	iuo_Transport
uo_camiones		iuo_Camion
uo_clienprove		iuo_Cliente
uo_binshuerto		iuo_Bins

Integer	ii_Tipo
String	is_rutclie

DataWindowChild	idwc_Cliente, idwc_Packing, idwc_Color
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean duplicado (string columna, string valor)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaingreso (string columna)
end prototypes

event ue_imprimir;Long		ll_Fila
String		ls_Informe
DateTime ld_desde, ld_Hasta

ld_desde = dw_2.Object.desp_fecdes[1]
ld_Hasta = dw_2.Object.desp_fecdes[1]

istr_info.Titulo	=	"INFORME DE DESPACHOS BINS"
istr_info.copias	=	1
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despachos"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Long(istr_Mant.Argumento[3]), ii_Tipo, -1, -1, -1, ld_Desde, ld_Hasta)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	IF gs_Ambiente = 'Windows' THEN
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	ELSE
		F_ImprimeInformePdf(vinf.dw_1, "TituloInforme")
	END IF
END IF
end event

event ue_imprimir2();Integer	li_planta, li_cliente, respuesta
Long		ll_despacho, ll_fila_e, ll_fila_d
String		ls_formato

SetPointer(HourGlass!)
dw_2.AcceptText()

li_planta 		=	dw_3.Object.plde_codigo[1]
li_cliente		=	dw_3.Object.clie_codigo[1]
ll_despacho	=	dw_3.Object.desp_numero[1]
//ll_despacho	=	dw_2.Object.defe_guides[1]

Long		fila
str_info	lstr_info

lstr_info.titulo	= "GUIA DE DESPACHO BINS HUERTO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_guia_despacho_gde"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(li_cliente, li_planta, ll_despacho,ii_Tipo, dw_3.Object.desp_guides[1])

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_3.Object.desp_fecdes.Protect	= 0
	dw_3.Object.desp_numero.Protect= 0
	dw_3.Object.cami_patent.Protect	= 0
	dw_3.Object.tran_codigo.Protect	= 0
	
	dw_3.Object.desp_fecdes.Color 	=	0
	dw_3.Object.desp_numero.Color	=	0
	dw_3.Object.cami_patent.Color	=	0
	dw_3.Object.tran_codigo.Color		=	0
	
	dw_3.Object.desp_fecdes.BackGround.Color 	= RGB(255,255,255)
	dw_3.Object.desp_numero.BackGround.Color	= RGB(255,255,255)
	dw_3.Object.cami_patent.BackGround.Color		= RGB(255,255,255)
	dw_3.Object.tran_codigo.BackGround.Color		= RGB(255,255,255)
	
	dw_3.Object.b_camion.Enabled = True
	If ii_Tipo = 1 Then 
		dw_3.Object.clpr_rut.Protect	=	0
		dw_3.Object.clpr_rut.Color 		=	0
		dw_3.Object.clpr_rut.BackGround.Color 	= RGB(255,255,255)
		dw_3.Object.b_cliente.Enabled = True
	End If
ELSE
	dw_3.Object.desp_fecdes.Protect	=	1
	dw_3.Object.desp_numero.Protect=	1
	dw_3.Object.cami_patent.Protect	= 1
	dw_3.Object.tran_codigo.Protect	= 1
	
	dw_3.Object.desp_fecdes.Color	=	RGB(255,255,255)
	dw_3.Object.desp_numero.Color	=	RGB(255,255,255)
	dw_3.Object.cami_patent.Color	=	RGB(255,255,255)
	dw_3.Object.tran_codigo.Color		=	RGB(255,255,255)

	dw_3.Object.desp_fecdes.BackGround.Color 	= 553648127
	dw_3.Object.desp_numero.BackGround.Color 	= 553648127
	dw_3.Object.cami_patent.BackGround.Color 	= 553648127
	dw_3.Object.tran_codigo.BackGround.Color 	= 553648127
	
	dw_3.Object.b_camion.Enabled = False
	If ii_Tipo = 1 Then 
		dw_3.Object.clpr_rut.Protect	= 1
		dw_3.Object.clpr_rut.Color		=	RGB(255,255,255)
		dw_3.Object.clpr_rut.BackGround.Color 	= 553648127
		
		dw_3.Object.b_cliente.Enabled = False
	End If
END IF
end subroutine

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_Codigo

li_Codigo	=	dw_1.Object.bins_numero[il_fila]

CHOOSE CASE columna
	CASE "bins_numero"
		li_Codigo	=	Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("bins_numero = " + String(li_Codigo), 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

Return false
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

wf_replicacion()

IF Borrando THEN
	IF dw_2.Update(True, False) = 1 THEN
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit
RETURN lb_Retorno
end function

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

If columna <> "tran_codigo" And &
	(dw_3.GetItemNumber(1, "tran_codigo") = 0 OR IsNull(dw_3.GetItemNumber(1, "tran_codigo"))) Then
	lb_estado = False
End If
	
If columna <> "cami_patent" And &
	(dw_3.GetItemString(1, "cami_patent") = "" OR IsNull(dw_3.GetItemString(1, "cami_patent"))) Then
	lb_estado = False
End If

If ii_Tipo = 1 Then
	If columna <> "clpr_rut" And &
		(dw_3.GetItemString(1, "clpr_rut") = "" OR IsNull(dw_3.GetItemString(1, "clpr_rut"))) Then
		lb_estado = False
	End If
End If

pb_grabar.Enabled = lb_estado
pb_ins_det.Enabled = lb_estado
end subroutine

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()
			
	ll_fila_e	= dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), ii_Tipo)
	ll_fila_e	= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), ii_Tipo)
	
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE
		DO
					
			ll_fila_d	= dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]), ii_Tipo)

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled	= True
				pb_ins_det.Enabled	= True
				pb_imprimir.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					If dw_3.Object.desp_guiemi[1] > 0 Then HabilitaEncab(False)
					
					If dw_3.Object.desp_guiemi[1] = 1 Then istr_mant.Solo_Consulta	=	True
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_3.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


end event

on w_maed_despachos.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.pb_guia=create pb_guia
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.pb_guia
this.Control[iCurrent+3]=this.dw_4
end on

on w_maed_despachos.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.pb_guia)
destroy(this.dw_4)
end on

event ue_nuevo;call super::ue_nuevo;dw_2.Reset()
dw_1.Reset()

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_3.SetFocus()

dw_3.GetChild('clie_codigo', idwc_Cliente)
dw_3.GetChild('plde_codigo', idwc_Packing)
dw_1.GetChild('ccev_codigo', idwc_Color)

idwc_Cliente.SetTransObject(SQLCA)
idwc_Packing.SetTransObject(SQLCA)
idwc_Color.SetTransObject(SQLCA)

idwc_Cliente.Retrieve()
idwc_Packing.Retrieve()
idwc_Color.Retrieve(-1)

dw_3.Object.clie_codigo[1] 		= gi_CodExport
dw_3.Object.plde_codigo[1] 	= gi_CodPlanta
dw_3.Object.cami_clasifi[1] 	= 1
dw_3.Object.desp_tipode[1]	=	ii_Tipo
dw_3.Object.desp_fecdes[1] 	= Today()

If ii_Tipo = 1 Then 
	dw_3.Object.b_cliente.Enabled 	= True
	dw_3.Object.clpr_rut.Protect		= 0
	dw_3.Object.b_cliente.Visible 		= True
	dw_3.Object.clpr_rut.Visible			= True
	dw_3.Object.clpr_rut_t.Visible		= True
	dw_1.Object.desp_preuni.Protect	= 0
	dw_1.Object.desp_pesnet.Protect	= 0
Else
	dw_3.Object.b_cliente.Enabled 	= False
	dw_3.Object.clpr_rut.Protect		= 1
	dw_3.Object.b_cliente.Visible 		= False
	dw_3.Object.clpr_rut.Visible			= False
	dw_3.Object.clpr_rut_t.Visible		= False
	dw_1.Object.desp_preuni.Protect	= 1
	dw_1.Object.desp_pesnet.Protect	= 1
End If

HabilitaEncab(True)
end event

event open;call super::open;
iuo_Despachos	=	Create uo_despachobins
iuo_Transport	=	Create uo_transportista
iuo_Camion		=	Create uo_camiones
iuo_Cliente		=	Create uo_clienprove
iuo_Bins			=	Create uo_binshuerto

dw_3.SetTransObject(SQLCA)
dw_4.SetTransObject(SQLCA)

This.Height	=	2700

ii_Tipo = Message.DoubleParm
istr_Mant.Argumento[1] = String(gi_CodExport)
istr_Mant.Argumento[2] = String(gi_CodPlanta)

If ii_Tipo = 1 Then
	This.Title = 'DESPACHO DE BINS - COMERCIALES'
Else
	This.Title = 'DESPACHO DE BINS - EXPORTACION'
End If
end event

event ue_nuevo_detalle;il_fila = dw_1.InsertRow(0)

IF il_fila > 0 THEN
	pb_eli_det.Enabled	= True
	pb_grabar.Enabled	= True
END IF

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn('bins_numero')
end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long				ll_Fila, ll_New, ll_Busca
dwItemStatus	l_status
String				ls_Busca


If IsNull(dw_3.Object.desp_numero[1]) Then
	dw_3.Object.desp_numero[1] = iuo_Despachos.of_Maximo(dw_3.Object.clie_codigo[1], dw_3.Object.plde_codigo[1], SQLCA)
End If

For ll_Fila = 1 To dw_1.RowCount()
	l_status = dw_1.GetItemStatus(ll_Fila, 0, Primary!)
		
	If l_status = NewModified! Or l_status = DataModified! THen 
		ls_Busca = 'bins_numero = ' + String(dw_1.Object.bins_numero[ll_Fila])
		ll_Busca = dw_2.Find(ls_Busca, 1, dw_2.RowCount())
		
		If ll_Busca = 0 Then
			ll_New = dw_2.InsertRow(0)
			
			dw_2.Object.clie_codigo[ll_New] 		= dw_3.Object.clie_codigo[1]
			dw_2.Object.plde_codigo[ll_New] 		= dw_3.Object.plde_codigo[1]
			dw_2.Object.desp_numero[ll_New]	= dw_3.Object.desp_numero[1]
			dw_2.Object.bins_numero[ll_New] 	= dw_1.Object.bins_numero[ll_Fila]
			dw_2.Object.desp_tipode[ll_New]		= dw_3.Object.desp_tipode[1]
			dw_2.Object.desp_fecdes[ll_New] 	= dw_3.Object.desp_fecdes[1]
			dw_2.Object.desp_pesnet[ll_New] 	= dw_1.Object.desp_pesnet[ll_Fila]
			dw_2.Object.desp_preuni[ll_New] 	= dw_1.Object.desp_preuni[ll_Fila]
		Else
			dw_2.Object.desp_pesnet[ll_Busca] 	= dw_1.Object.desp_pesnet[ll_Fila]
			dw_2.Object.desp_preuni[ll_Busca] 	= dw_1.Object.desp_preuni[ll_Fila]
		End If
	End If
Next

For ll_Fila = 1 to dw_2.RowCount()
	dw_2.Object.desp_guiemi[ll_Fila] 	= dw_3.Object.desp_guiemi[1]
	dw_2.Object.desp_guides[ll_Fila] 	= dw_3.Object.desp_guides[1]
	dw_2.Object.cami_clasifi[ll_Fila] 	= dw_3.Object.cami_clasifi[1]
	dw_2.Object.cami_patent[ll_Fila] 	= dw_3.Object.cami_patent[1]
	dw_2.Object.clpr_rut[ll_Fila] 		= dw_3.Object.clpr_rut[1]
	dw_2.Object.tran_codigo[ll_Fila] 	= dw_3.Object.tran_codigo[1]
	dw_2.Object.desp_usuari[ll_Fila] 	= gstr_us.Nombre
	dw_2.Object.desp_comput[ll_Fila] = gstr_us.Computador
Next

For ll_Fila = 1 to dw_1.DeletedCount()
	ls_Busca = 'bins_numero = ' + String(dw_1.GetItemNumber(ll_Fila, 'bins_numero', Delete!, False))
	ll_Busca = dw_2.Find(ls_Busca, 1, dw_2.RowCount())
	
	If ll_Busca > 0 Then dw_2.DeleteRow(ll_Busca)
Next
end event

event resize;call super::resize;dw_3.x = dw_2.x
dw_3.y = dw_2.y
dw_3.Width = dw_2.Width
dw_3.Height = dw_2.Height

pb_guia.x	=	pb_salir.x
pb_guia.y	=	pb_salir.y + 255

end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

//IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,2,Delete!)  = 1 THEN
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

event ue_seleccion;call super::ue_seleccion;str_Busqueda	lstr_Busq

lstr_Busq.argum[1] = String(dw_3.Object.clie_codigo[1])
lstr_Busq.argum[2] = String(dw_3.Object.plde_codigo[1])
lstr_Busq.argum[3] = String(ii_Tipo)

OpenWithParm(w_busc_despachos, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) = 4 Then
	If lstr_Busq.argum[1] <> "" Then
		istr_Mant.Argumento[1] = lstr_Busq.argum[1]
		istr_Mant.Argumento[2] = lstr_Busq.argum[2]
		istr_Mant.Argumento[3] = lstr_Busq.argum[3]
		istr_Mant.Argumento[4] = lstr_Busq.argum[4]
		This.TriggerEvent("ue_recuperadatos")
	Else
		pb_buscar.SetFocus()
	End If
End If
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_despachos
integer x = 142
integer y = 876
integer width = 2775
integer height = 1168
integer taborder = 100
string title = "Detalle de BINS"
string dataobject = "dw_mant_mues_despachobins"
end type

event dw_1::itemchanged;call super::itemchanged;String ls_Columna, ls_Null

SetNull(ls_NUll)
ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "bins_numero"
		If Duplicado(ls_Columna, Data) Or &
			Not iuo_Bins.of_Existe(gi_CodExport, gi_CodPlanta, Long(Data), True, SQLCA) Then
			SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		Else
			If iuo_Bins.Estado <> 1 Then
				MessageBox('Atencion', 'BINS no se encuentra en existencia.', StopSign!, Ok!)
				SetItem(Row, ls_Columna, Long(ls_Null))
				Return 1
			Else
				If (ii_Tipo = 1 And iuo_Bins.Categoria = 120) Or  (ii_Tipo = 2 And iuo_Bins.Categoria = 100) Then 
					This.Object.ccev_codigo[Row]	= iuo_Bins.Color
					This.Object.cate_codigo[Row]	= iuo_Bins.Categoria
					This.Object.bins_grucal[Row]	= iuo_Bins.Grupo
					This.Object.bins_pesnet[Row] 	= iuo_Bins.PesoNeto
				Else
					MessageBox('Atencion', 'BINS no corresponde a categoria.', StopSign!, Ok!)
					SetItem(Row, ls_Columna, Long(ls_Null))
					Return 1
				End If
			End If
		End If
		
End Choose
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_despachos
boolean visible = false
integer x = 101
integer y = 60
integer width = 2674
integer height = 760
string dataobject = "dw_gene_despachos"
end type

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_despachos
integer x = 3195
integer y = 228
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_despachos
integer x = 3200
integer y = 456
integer width = 297
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_despachos
integer x = 3195
integer y = 664
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_despachos
integer x = 3195
integer y = 788
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_despachos
integer x = 3195
integer y = 1136
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_despachos
integer x = 3195
integer y = 1460
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_despachos
integer x = 3195
integer y = 1636
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_despachos
integer x = 3195
integer y = 52
end type

type dw_3 from uo_dw within w_maed_despachos
integer x = 119
integer y = 60
integer width = 2674
integer height = 760
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_despachos"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;String	ls_columna, ls_Null

SetNull(ls_Null)
ls_columna = dwo.Name

Choose Case ls_columna
	Case	"desp_numero"
		If Not iuo_Despachos.of_Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], ii_Tipo, Long(Data), False, SQLCA) Then
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		Else
			istr_Mant.Argumento[3] = Data
			Parent.TriggerEvent('ue_Recuperadatos')
		End If
		
	Case "tran_codigo"
		If Not iuo_Transport.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(row, ls_Columna, Integer(ls_Null))
			Return 1
		End If

	Case "cami_patent"
		If data <> "" And Not iuo_Camion.Existe(1, Data, True, sqlca) Then
			This.SetItem(row, ls_Columna, ls_Null)
			
			Return 1
		Else
			This.Object.cami_patcar[row]	=	iuo_Camion.PateCarro
			This.Object.cami_rutcho[row]	=	iuo_Camion.RutChofer
			This.Object.cami_chofer[row]	=	iuo_Camion.Chofer
		End If
	
	Case "clpr_rut"
		is_rutclie = F_verrut(data, True)
		If is_rutclie = "" Then
			This.SetItem(1, "clpr_rut", ls_Null)
			Return 1
		Else
			If Not iuo_cliente.existe(is_rutclie, true, sqlca) Then
				This.SetItem(1, "clpr_rut", ls_Null)
				Return 1
			Else
				This.SetItem(1, "clpr_rut", is_rutclie)
				This.SetItem(1, "clpr_nombre", iuo_Cliente.RazonSocial)
			End If
		End If
		
End Choose

HabilitaIngreso(ls_columna)
end event

event itemerror;call super::itemerror;Return 1
end event

event buttonclicked;call super::buttonclicked;Str_busqueda	lstr_busq

Choose Case dwo.Name
	Case "b_camion"
		lstr_busq.argum[1] = '1'
		
		OpenWithParm(w_busc_camiones, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		If lstr_busq.argum[1] = "" Then
			dw_3.SetColumn("cami_patent")
			dw_3.SetFocus()
		Else
			dw_3.Object.cami_clasifi[il_fila]	=	Integer(lstr_busq.argum[1])
			dw_3.Object.cami_patent[il_fila]		=	lstr_busq.argum[2]
			dw_3.Object.cami_patcar[il_fila]		=	lstr_busq.argum[6]
			dw_3.Object.cami_rutcho[il_fila]		=	lstr_busq.argum[5]
			dw_3.Object.cami_chofer[il_fila]		=	lstr_busq.argum[4]
			dw_3.SetFocus()
		End If

		iuo_Camion.Existe(1, This.Object.cami_patent[row], True, sqlca)
		HabilitaIngreso('cami_patent')
		
	Case 'b_cliente'
		lstr_busq.argum[1] = '0'
		
		OpenWithParm(w_busc_clienprove, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		If UpperBound(lstr_busq.argum) < 2 Then Return
		
		If lstr_busq.argum[1] = "" Then
			dw_3.SetColumn("clpr_rut")
			dw_3.SetFocus()
		Else
			dw_3.Object.clpr_rut[il_fila]			=	lstr_busq.argum[1]
			dw_3.Object.clpr_nombre[il_fila]	=	lstr_busq.argum[2]
			dw_3.SetFocus()
		End If
		HabilitaIngreso("clpr_rut")
End Choose
end event

type pb_guia from picturebutton within w_maed_despachos
integer x = 3168
integer y = 1040
integer width = 302
integer height = 244
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Guia.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guia-bn.png"
alignment htextalign = left!
end type

event clicked;Long 		ll_Guia, ll_Despacho, Fila
Integer	li_Planta, li_Cliente
str_info	lstr_info

uo_GuiaDespacho	iuo_Guia
iuo_Guia = Create uo_GuiaDespacho

If IsNull(dw_3.Object.cami_patent[1]) Or dw_3.Object.cami_patent[1] = "" Then
	Destroy iuo_Guia
	Return
End If

If IsNull(dw_3.Object.tran_codigo[1]) Then
	Destroy iuo_Guia
	Return
End If

If ii_Tipo = 1 Then
	If IsNull(dw_3.Object.clpr_rut[1]) Or dw_3.Object.clpr_rut[1] = "" Then
		Destroy iuo_Guia
		Return
	End If
End If

li_Planta 		=	dw_3.Object.plde_codigo[1]
li_Cliente 	=	dw_3.Object.clie_codigo[1]
ll_Despacho	=	dw_3.Object.desp_numero[1]

dw_4.Retrieve(li_Cliente, li_Planta, ll_Despacho, ii_Tipo, 1)

//If gi_Emisor_Electronico = 1 Then
If dw_3.Object.desp_guiemi[1] = 0 Then 
	ll_Guia = iuo_Guia.of_EmiteGuia_FruticolaGDE(li_Planta, li_Cliente, ll_Despacho, ii_Tipo)
	If ll_Guia > 0 Then
		If Not iuo_Guia.of_generalibroguia_gde(ii_Tipo) Then 
			MessageBox('Alerta', 'No se pudo actualziar Libro de guias de despacho.', Information!, OK!)
		End If
		dw_3.Object.desp_guides[1] = ll_Guia
		dw_3.Object.desp_guiemi[1] = 1
		Parent.TriggerEvent('ue_guardar')
		
		iuo_Guia.of_RecuperaPDF(ll_Guia, dw_4.Object.defe_fecdes[1], 1)
		Parent.PostEvent("ue_imprimir2")
	End If
Else
	iuo_Guia.of_RecuperaPDF(dw_3.Object.desp_guides[1], dw_4.Object.defe_fecdes[1], 1)
	Parent.PostEvent("ue_imprimir2")
End If

Destroy iuo_Guia
end event

type dw_4 from uo_dw within w_maed_despachos
boolean visible = false
integer x = 2981
integer y = 1852
integer width = 192
integer height = 144
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_info_guia_despacho_gde"
boolean vscrollbar = false
boolean border = false
end type

