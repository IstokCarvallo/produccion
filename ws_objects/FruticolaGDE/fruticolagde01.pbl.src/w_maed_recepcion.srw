$PBExportHeader$w_maed_recepcion.srw
forward
global type w_maed_recepcion from w_mant_encab_deta
end type
type dw_3 from uo_dw within w_maed_recepcion
end type
end forward

global type w_maed_recepcion from w_mant_encab_deta
integer width = 3570
integer height = 2128
string title = "RECEPCION DE BINS"
string menuname = ""
event ue_imprimir ( )
dw_3 dw_3
end type
global w_maed_recepcion w_maed_recepcion

type variables
uo_ProdCuarteles	iuo_Cuarteles 
uo_prodPredio		iuo_Predio
uo_recepcionbins	iuo_Recepcion
uo_despachobins	iuo_Despachos

DataWindowChild	idwc_Cliente, idwc_Packing, idwc_Productor, idwc_Predio, idwc_Cuartel, idwc_Especie, idwc_Variedad, idwc_Color
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean duplicado (string columna, string valor)
protected function boolean wf_actualiza_db (boolean borrando)
end prototypes

event ue_imprimir;Long		ll_Fila
String		ls_Informe
DateTime ld_desde, ld_Hasta

ld_desde = dw_2.Object.rece_fecrec[1]
ld_Hasta = dw_2.Object.rece_fecrec[1]

istr_info.Titulo	=	"INFORME DE RECEPCIONES BINS"
istr_info.copias	=	1
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_recepciones"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila = vinf.dw_1.Retrieve(gi_CodExport, gi_CodPlanta, dw_3.Object.rece_numero[1], -1, -1, -1, ld_Desde, ld_Hasta)

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

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_3.Object.prpr_codigo.Protect	= 0
	dw_3.Object.prcc_codigo.Protect	= 0
//	dw_3.Object.rece_fecrec	.Protect	= 0
	dw_3.Object.rece_numero.Protect	= 0
	dw_3.Object.bins_pesnet.Protect	= 0
	
	dw_3.Object.prpr_codigo.Color 	=	0
	dw_3.Object.prcc_codigo.Color 	=	0
//	dw_3.Object.rece_fecrec.Color 	=	0
	dw_3.Object.rece_numero.Color	=	0
	dw_3.Object.bins_pesnet.Color 	=	0

	dw_3.Object.prpr_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_3.Object.prcc_codigo.BackGround.Color 	= RGB(255,255,255)
//	dw_3.Object.rece_fecrec.BackGround.Color 	= RGB(255,255,255)
	dw_3.Object.rece_numero.BackGround.Color	= RGB(255,255,255)
	dw_3.Object.bins_pesnet.BackGround.Color 	= RGB(255,255,255)
ELSE
	dw_3.Object.prpr_codigo.Protect	=	1
	dw_3.Object.prcc_codigo.Protect	=	1
//	dw_3.Object.rece_fecrec	.Protect	=	1
	dw_3.Object.rece_numero.Protect	=	1
	dw_3.Object.bins_pesnet.Protect	=	1
	
	dw_3.Object.prpr_codigo.Color		=	RGB(255,255,255)
	dw_3.Object.prcc_codigo.Color		=	RGB(255,255,255)
//	dw_3.Object.rece_fecrec.Color		=	RGB(255,255,255)
	dw_3.Object.rece_numero.Color	=	RGB(255,255,255)
	dw_3.Object.bins_pesnet.Color 	=	RGB(255,255,255)

	dw_3.Object.prpr_codigo.BackGround.Color 	= 553648127
	dw_3.Object.prcc_codigo.BackGround.Color 	= 553648127
//	dw_3.Object.rece_fecrec.BackGround.Color 	= 553648127
	dw_3.Object.rece_numero.BackGround.Color 	= 553648127
	dw_3.Object.bins_pesnet.BackGround.Color 	= 553648127
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
dw_2.GrupoFecha	=	ldt_FechaHora

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

wf_replicacion()

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
END IF

sqlca.AutoCommit	=	lb_AutoCommit
RETURN lb_Retorno
end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()
			
	ll_fila_e	= dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]))
	ll_fila_e	= dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]))
	
	dw_3.GetChild('prcc_codigo', idwc_Cuartel)
	idwc_Cuartel.SetTransObject(SQLCA)
	idwc_Cuartel.Retrieve(gstr_parempresa.Productor, dw_3.Object.prpr_codigo[1])
	
	dw_3.GetChild('vari_codigo', idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_3.Object.espe_codigo[1])
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE
		DO
					
			ll_fila_d	= dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]), Integer(istr_Mant.Argumento[3]))

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
					
					idwc_Color.Retrieve(dw_3.Object.espe_codigo[1])
					HabilitaEncab(False)
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

on w_maed_recepcion.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_recepcion.destroy
call super::destroy
destroy(this.dw_3)
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
dw_3.GetChild('prod_codigo', idwc_Productor)
dw_3.GetChild('prpr_codigo', idwc_Predio)
dw_3.GetChild('prcc_codigo', idwc_Cuartel)
dw_3.GetChild('espe_codigo', idwc_Especie)
dw_3.GetChild('vari_codigo', idwc_Variedad)
dw_1.GetChild('ccev_codigo', idwc_Color)

idwc_Cliente.SetTransObject(SQLCA)
idwc_Packing.SetTransObject(SQLCA)
idwc_Productor.SetTransObject(SQLCA)
idwc_Predio.SetTransObject(SQLCA)
idwc_Cuartel.SetTransObject(SQLCA)
idwc_Especie.SetTransObject(SQLCA)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Color.SetTransObject(SQLCA)

idwc_Cliente.Retrieve()
idwc_Packing.Retrieve()
idwc_Productor.Retrieve(-1)
idwc_Predio.Retrieve(gstr_parempresa.Productor)
idwc_Cuartel.Retrieve(gstr_parempresa.Productor, -1)
idwc_Especie.Retrieve()
idwc_Variedad.Retrieve(-1)
idwc_Color.Retrieve(-1)

dw_3.Object.clie_codigo[1] 		= gi_CodExport
dw_3.Object.plde_codigo[1] 	= gi_CodPlanta
dw_3.Object.prod_codigo[1]	= gstr_parempresa.Productor
dw_3.Object.rece_fecrec[1] 	= Today()
dw_3.Object.bins_pesnet[1] 	= gstr_parempresa.PesoNeto
dw_3.Object.prpr_codigo[1] 	= 1

HabilitaEncab(True)
end event

event open;call super::open;iuo_Cuarteles	=	Create uo_ProdCuarteles
iuo_Predio		=	Create uo_prodPredio
iuo_Recepcion	=	Create uo_recepcionbins
iuo_Despachos	=	Create uo_despachobins

dw_3.SetTransObject(SQLCA)

istr_Mant.Argumento[1] = String(gi_CodExport)
istr_Mant.Argumento[2] = String(gi_CodPlanta)
end event

event ue_nuevo_detalle;il_fila = dw_1.InsertRow(0)

If gstr_parempresa.Tote = 0 Then
	dw_1.Object.bins_ccajas.Visible = False	
	dw_1.Object.bins_ccajas_t.Visible = False	
End If

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

If Not iuo_Despachos.of_ExisteBins(dw_3.Object.clie_codigo[1], dw_3.Object.plde_codigo[1], dw_1.Object.bins_numero[dw_1.GetRow()],SQLCA) Then Return 

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


If IsNull(dw_3.Object.rece_numero[1]) Then
	dw_3.Object.rece_numero[1] = iuo_Recepcion.of_Maximo(dw_3.Object.clie_codigo[1], dw_3.Object.plde_codigo[1], SQLCA)
End If

For ll_Fila = 1 To dw_1.RowCount()
	l_status = dw_1.GetItemStatus(ll_Fila, "bins_numero", Primary!)
		
	If l_status = NewModified! Or l_status = DataModified! THen 
		dw_1.Object.clie_codigo[ll_Fila] 	= dw_3.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_Fila] 	= dw_3.Object.plde_codigo[1]
		dw_1.Object.espe_codigo[ll_Fila] 	= dw_3.Object.espe_codigo[1]
		dw_1.Object.vari_codigo[ll_Fila] 	= dw_3.Object.vari_codigo[1]
		dw_1.Object.prod_codigo[ll_Fila] 	= dw_3.Object.prod_codigo[1]
		dw_1.Object.prpr_codigo[ll_Fila] 	= dw_3.Object.prpr_codigo[1]
		dw_1.Object.prcc_codigo[ll_Fila] 	= dw_3.Object.prcc_codigo[1]
		dw_1.Object.bins_estado[ll_Fila] 	= 1
		
		If gstr_parempresa.Tote = 0 Then
			dw_1.Object.bins_pesnet[ll_Fila] 	= dw_3.Object.bins_pesnet[1]
			dw_1.Object.bins_ccajas[ll_Fila]	= 0
		Else
			dw_1.Object.bins_pesnet[ll_Fila] 	=  dw_1.Object.bins_ccajas[ll_Fila] * gstr_parempresa.PesoNeto
		End If
		
		dw_1.Object.bins_fechac[ll_Fila] 	= Today()
		
		ll_New = dw_2.InsertRow(0)
		
		dw_2.Object.clie_codigo[ll_New] 		= dw_3.Object.clie_codigo[1]
		dw_2.Object.plde_codigo[ll_New] 		= dw_3.Object.plde_codigo[1]
		dw_2.Object.rece_numero[ll_New]	= dw_3.Object.rece_numero[1]
		dw_2.Object.bins_numero[ll_New] 	= dw_1.Object.bins_numero[ll_Fila]
		dw_2.Object.rece_fecrec[ll_New] 		= dw_3.Object.rece_fecrec[1]
		dw_2.Object.cont_codigo[ll_New] 		= dw_3.Object.cont_codigo[1]
		dw_2.Object.rece_pdaopc[ll_New] 	= 1
	End If
Next

l_status = dw_3.GetItemStatus(1, "rece_fecrec", Primary!)
If l_status = DataModified! Then 
	For ll_Fila = 1 To dw_2.RowCount()
		dw_2.Object.rece_fecrec[ll_Fila]	=	dw_3.Object.rece_fecrec[1]
		dw_2.Object.rece_usuari[ll_Fila]	=	dw_3.Object.rece_usuari[1]
		dw_2.Object.rece_comput[ll_Fila]	=	dw_3.Object.rece_comput[1]
		dw_2.Object.rece_camfec[ll_Fila]	=	dw_3.Object.rece_camfec[1]
	Next 
	
	For ll_Fila = 1 To dw_1.RowCount()
		dw_1.Object.bins_fechac[ll_Fila]	=	dw_3.Object.rece_fecrec[1]
	Next	
End If

For ll_Fila = 1 to dw_1.DeletedCount()
	ls_Busca = 'bins_numero = ' + String(dw_1.GetItemNumber(ll_Fila, 'bins_numero', Delete!, False))
	ll_Busca = dw_2.Find(ls_Busca, 1, dw_2.RowCount())
	
	If ll_Busca > 0 Then dw_2.DeleteRow(ll_Busca)
NExt
end event

event resize;call super::resize;dw_3.x = dw_2.x
dw_3.y = dw_2.y
dw_3.Width = dw_2.Width
dw_3.Height = dw_2.Height
end event

event ue_borrar;Long ll_Fila

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

For ll_Fila = 1 To dw_1.RowCount()
	If Not iuo_Despachos.of_ExisteBins(dw_3.Object.clie_codigo[1], dw_3.Object.plde_codigo[1], dw_1.Object.bins_numero[ll_Fila],SQLCA) Then Return 
Next

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

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

OpenWithParm(w_busc_recepciones, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) = 3 Then
	If lstr_Busq.argum[1] <> "" Then
		istr_Mant.Argumento[1] = lstr_Busq.argum[1]
		istr_Mant.Argumento[2] = lstr_Busq.argum[2]
		istr_Mant.Argumento[3] = lstr_Busq.argum[3]
		This.TriggerEvent("ue_recuperadatos")
	Else
		pb_buscar.SetFocus()
	End If
End If
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_recepcion
integer x = 101
integer y = 944
integer width = 2903
integer height = 1052
integer taborder = 100
string title = "Detalle de BINS"
string dataobject = "dw_mant_mues_recepcionbins"
end type

event dw_1::itemchanged;call super::itemchanged;String ls_Columna, ls_Null

SetNull(ls_NUll)
ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "bins_numero"
		If Duplicado(ls_Columna, Data) Or &
			Not iuo_Recepcion.of_ExisteBins(gi_CodExport, gi_CodPlanta, Long(Data), SQLCA) Then
			SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		End If
		
End Choose
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_recepcion
boolean visible = false
integer x = 183
integer y = 60
integer width = 2898
integer height = 764
string dataobject = "dw_gene_recepcion"
end type

event dw_2::clicked;//
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_recepcion
integer x = 3195
integer y = 228
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_recepcion
integer x = 3200
integer y = 456
integer width = 297
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_recepcion
integer x = 3195
integer y = 664
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_recepcion
integer x = 3195
integer y = 788
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_recepcion
integer x = 3195
integer y = 1136
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_recepcion
integer x = 3195
integer y = 1460
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_recepcion
integer x = 3195
integer y = 1636
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_recepcion
integer x = 3195
integer y = 52
end type

type dw_3 from uo_dw within w_maed_recepcion
integer x = 183
integer y = 60
integer width = 2898
integer height = 764
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_recepcion"
boolean vscrollbar = false
end type

event itemchanged;call super::itemchanged;String	ls_columna, ls_Null

SetNull(ls_Null)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	Case	"rece_numero"
		If Not iuo_Recepcion.of_Existe(This.Object.clie_codigo[1], This.Object.plde_codigo[1], Long(Data), False, SQLCA) Then
			This.SetItem(Row, ls_Columna, Long(ls_Null))
			Return 1
		Else
			istr_Mant.Argumento[3] = Data
			Parent.TriggerEvent('ue_Recuperadatos')
		End If
		
	Case "prpr_codigo"
		If Not iuo_Predio.Existe(Integer(Data), This.Object.prod_codigo[1], True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else
			dw_2.GetChild('prcc_codigo', idwc_Cuartel)
			dw_2.GetChild('vari_codigo', idwc_Variedad)
			dw_1.GetChild('ccev_codigo', idwc_Color)
			
			idwc_Cuartel.SetTransObject(SQLCA)
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Color.SetTransObject(SQLCA)
			
			idwc_Cuartel.Retrieve(This.Object.prod_codigo[1], iuo_Predio.Codigo)
			idwc_Variedad.Retrieve(-1)
			idwc_Color.Retrieve(-1)
		End If
		
	Case "prcc_codigo"
		If Not iuo_Cuarteles.Existe(This.Object.prod_codigo[1], This.Object.prpr_codigo[1], Integer(Data), True, SQLCA) Then 
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			This.SetItem(Row, 'espe_codigo', Integer(ls_Null))
			This.SetItem(Row, 'vari_codigo', Integer(ls_Null))
			Return 1
		Else
			This.SetItem(Row, 'espe_codigo', iuo_Cuarteles.Especie)
			This.SetItem(Row, 'vari_codigo', iuo_Cuarteles.variedad)
			
			dw_2.GetChild('vari_codigo', idwc_Variedad)
			dw_1.GetChild('ccev_codigo', idwc_Color)
			
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Color.SetTransObject(SQLCA)
			
			idwc_Variedad.Retrieve(iuo_Cuarteles.Especie)
			idwc_Color.Retrieve(iuo_Cuarteles.Especie)
		End If
		
	Case "rece_fecrec"
		This.Object.rece_usuari[Row] =	gstr_Us.Nombre
		This.Object.rece_comput[Row] =	gstr_Us.Computador
		This.Object.rece_camfec[Row] =	1

END CHOOSE

pb_ins_det.Enabled = True
end event

event itemerror;call super::itemerror;Return 1
end event

