$PBExportHeader$w_carga_archivo_destino.srw
forward
global type w_carga_archivo_destino from window
end type
type dw_2 from uo_dw within w_carga_archivo_destino
end type
type sle_registro from singlelineedit within w_carga_archivo_destino
end type
type sle_archivo from singlelineedit within w_carga_archivo_destino
end type
type st_2 from statictext within w_carga_archivo_destino
end type
type st_1 from statictext within w_carga_archivo_destino
end type
type pb_imprimir from picturebutton within w_carga_archivo_destino
end type
type pb_archivo from picturebutton within w_carga_archivo_destino
end type
type dw_1 from uo_dw within w_carga_archivo_destino
end type
type pb_salir from picturebutton within w_carga_archivo_destino
end type
type pb_grabar from picturebutton within w_carga_archivo_destino
end type
end forward

global type w_carga_archivo_destino from window
integer width = 3634
integer height = 2300
boolean titlebar = true
string title = "Carga  Planilla Destino"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
string icon = "TABLA.ICO"
event ue_guardar pbm_custom11
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle ( )
event ue_listo pbm_custom28
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
dw_2 dw_2
sle_registro sle_registro
sle_archivo sle_archivo
st_2 st_2
st_1 st_1
pb_imprimir pb_imprimir
pb_archivo pb_archivo
dw_1 dw_1
pb_salir pb_salir
pb_grabar pb_grabar
end type
global w_carga_archivo_destino w_carga_archivo_destino

type prototypes
FUNCTION Boolean MoveFileA(Ref String ipExistingFileName, Ref String ipNewFileName ) LIBRARY "KERNEL32.DLL" alias for "MoveFileA;Ansi"
end prototypes

type variables
protected:
Long		il_fila
Boolean	ib_datos_ok, ib_borrar, ib_ok
Menu		im_menu
str_mant istr_mant
Str_parms	istr_parms
Str_info		istr_info
Str_mant		istr_mant2
end variables

forward prototypes
protected function integer wf_modifica ()
public function boolean validadetalle (integer ai_cliente, long al_pallet, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_calibr, long al_cajas, string as_errores)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexistedetalle (integer ai_cliente, string as_tipotr, integer ai_nave, integer ai_especie, integer ai_variedad, integer ai_etiqueta, integer ai_productor, string as_calibr, string as_embalaje, date ad_fecha, integer ai_secuen, string as_errores)
public function boolean validaencabezado (integer ai_especie, integer ai_variedad, integer ai_productor, integer ai_inspector, integer ai_etiqueta, integer ai_puerto, integer ai_recibidor, long ll_nuevo, string as_errores)
end prototypes

event type long ue_guardar(unsignedlong wparam, long lparam);Boolean	lb_retor

IF dw_1.AcceptText() = -1 THEN RETURN 0

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	RETURN 0
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN 1
END IF
end event

event ue_carga_detalle();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Long			ll_nuevo
String		ls_datos, ls_emba, ls_calibr, ls_productor, ls_errores, ls_malo, ls_tipotran, &
				ls_bodega, ls_lugarinsp, ls_reslot, ls_palleti, &
				ls_aparie, ls_forade, ls_forap, ls_pesras, ls_pesrab, ls_recidu, ls_mancha, ls_desmod, &
				ls_desalt, ls_deslev, ls_coblan, ls_cocris, ls_codesp, ls_conso2, ls_cobapa, ls_codesg, &
				ls_pardea, ls_baacuo, ls_conden, ls_resemb, ls_rescal, ls_rescon, ls_rescaj, ls_nomesp, &
				ls_varinom, ls_nometiq, ls_nomprod, ls_nominsp, ls_nompuer, ls_nomreci
Date			ld_feemb, ld_ferec, ld_fumigacion, ld_arribo, ld_inspeccion
Integer		li_retorno = 2, li_archivo, li_cliente = 81, li_cate, li_tipo, li_nave,&
				li_espe, li_vari, li_prod, li_cond, li_etiq, li_estado, li_ccalid, li_mensaje, &
				li_inspector, li_puerto, li_recibidor, li_cauobj1, li_cauobj2, li_cauobj3, &
				li_secuen, li_nropaq, li_nrorac, li_cobay1, li_cobay2, li_cobay3, li_cobay4, &
				li_tamba1, li_tamba2, li_tamba3, li_tamba4, li_tamba5, li_punuba, li_punini, &
				li_punura, li_pacida, li_opnbay, li_dafrio, li_coprem, li_caucajl
Decimal{1}	lde_temmax, lde_temmin, lde_temfre							
li_archivo	= FileOpen(sle_archivo.text)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

IF li_archivo < 0 THEN
	li_retorno				= MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!,RetryCancel!)
	Message.DoubleParm	= li_retorno
	RETURN
ELSE
	SetPointer(HourGlass!)
END IF

DO WHILE FileRead(li_archivo, ls_datos) >= 0
	IF Mid(ls_datos,1,1) = '1' THEN
		ls_errores		= 	''
		ls_tipotran 	=	Mid(ls_datos,2,1)					//	Tipo de Transporte
		li_nave	 		=	Integer(Mid(ls_datos,3,3))		// Cód. Nave
		li_espe			=	Integer(Mid(ls_datos,6,2))		//	Especie			
		li_vari			=	Integer(Mid(ls_datos,8,4))		//	Variedad			
		li_etiq			=	Integer(Mid(ls_datos,12,2))	//	Etiqueta			
		li_prod			=	Integer(Mid(ls_datos,14,3))	// Productor		
		ls_calibr		=	Mid(ls_datos,17,3)				// Calibre
		ls_emba			=	Trim(Mid(ls_datos,20,10))		//	Embalaje
		ld_feemb			=	Date(Mid(ls_datos,30,2) + "/" + &
								Mid(ls_datos,32,2) + "/" + &
								Mid(ls_datos,34,4))				//	Fecha Embalaje
		li_inspector	=	Integer(Mid(ls_datos,38,4))	// Inspector			
		li_puerto		=	Integer(Mid(ls_datos,42,3))	// Puerto			
		li_recibidor	=	Integer(Mid(ls_datos,45,3))	// Recibidor			
		ls_bodega		=	Mid(ls_datos,48,15)				// Bodega
		ls_lugarinsp	=	Mid(ls_datos,63,10)				// Lugar de Inspección
		ld_arribo		=	Date(Mid(ls_datos,73,2) + "/" + &
								Mid(ls_datos,75,2) + "/" + &
								Mid(ls_datos,77,4))				// Fecha Arribo
      ld_fumigacion	=	Date(Mid(ls_datos,81,2) + "/" + &
								Mid(ls_datos,83,2) + "/" + &
								Mid(ls_datos,85,4))				// Fecha Fumigacion	
		ld_inspeccion	=	Date(Mid(ls_datos,89,2) + "/" + &
								Mid(ls_datos,91,2) + "/" + &
								Mid(ls_datos,93,4))				// Fecha Inspección
		lde_temmax		=	dec(Mid(ls_datos,97,4))
		lde_temmin		=	Dec(Mid(ls_datos,101,4))
		lde_temfre		=	Dec(Mid(ls_datos,105,4))
		ls_reslot		=	Mid(ls_datos,109,1)
		li_cauobj1		=	Integer(Mid(ls_datos,110,3))
		li_cauobj2		=	Integer(Mid(ls_datos,113,3))
		li_cauobj3		=	Integer(Mid(ls_datos,116,3))
		
		sle_registro.Text	=	" Procesando Archivo Planilla Destino"
		
		li_mensaje = 2
		DO
				IF ValidaEncabezado(li_espe, li_vari, li_prod, li_inspector, li_etiq, &
										  li_puerto, li_recibidor, ll_nuevo,ls_errores) THEN
					li_mensaje	=	1
				ELSE
					li_mensaje	=	2
				END IF	
		LOOP WHILE	li_mensaje = 2
		
		ll_nuevo = dw_2.InsertRow(0)
		
		dw_2.SetItem(ll_nuevo, "clie_codigo", li_cliente)		
		dw_2.SetItem(ll_nuevo, "nave_tipotr", ls_tipotran)
		dw_2.SetItem(ll_nuevo, "nave_codigo", li_nave)			
		dw_2.SetItem(ll_nuevo, "espe_codigo", li_espe)
		dw_2.SetItem(ll_nuevo, "espe_nombre", istr_Mant.Argumento[1])
		dw_2.SetItem(ll_nuevo, "vari_codigo", li_vari)
		dw_2.SetItem(ll_nuevo, "vari_nombre", istr_Mant.Argumento[4])
		dw_2.SetItem(ll_nuevo, "etiq_codigo", li_etiq)
		dw_2.SetItem(ll_nuevo, "etiq_nombre", istr_Mant.Argumento[2])
		dw_2.SetItem(ll_nuevo, "prod_codigo", li_prod)
		dw_2.SetItem(ll_nuevo, "prod_nombre", istr_Mant.Argumento[3]) 
		dw_2.SetItem(ll_nuevo, "vaca_calibr", ls_calibr)
		dw_2.SetItem(ll_nuevo, "emba_codigo", ls_emba)
		dw_2.SetItem(ll_nuevo, "ccde_fecemb", ld_feemb)
		dw_2.SetItem(ll_nuevo, "ccin_codigo", li_inspector)
		dw_2.SetItem(ll_nuevo, "ccin_nombre", istr_Mant.Argumento[5])
		dw_2.SetItem(ll_nuevo, "puer_codigo", li_puerto)
		dw_2.SetItem(ll_nuevo, "puer_nombre", istr_Mant.Argumento[6])
		dw_2.SetItem(ll_nuevo, "reci_codigo", li_recibidor)
		dw_2.SetItem(ll_nuevo, "reci_nombre", istr_Mant.Argumento[7])
		dw_2.SetItem(ll_nuevo, "ccde_bodega", ls_bodega)
		dw_2.SetItem(ll_nuevo, "ccde_lugins", ls_lugarinsp)
		dw_2.SetItem(ll_nuevo, "ccde_fecarr", ld_arribo)
		dw_2.SetItem(ll_nuevo, "ccde_fecfum", ld_fumigacion)
		dw_2.SetItem(ll_nuevo, "ccde_fecins", ld_inspeccion)
		dw_2.SetItem(ll_nuevo, "ccde_temmax", lde_temmax)
		dw_2.SetItem(ll_nuevo, "ccde_temmin", lde_temmin)
		dw_2.SetItem(ll_nuevo, "ccde_temfre", lde_temfre)
		dw_2.SetItem(ll_nuevo, "ccde_reslot", ls_reslot)
		dw_2.SetItem(ll_nuevo, "ccde_cauobj", li_cauobj1)
		dw_2.SetItem(ll_nuevo, "ccde_causa2", li_cauobj2)
		dw_2.SetItem(ll_nuevo, "ccde_causa3", li_cauobj3)	
		
		
		
	ELSEIF Mid(ls_datos,1,1) = '2' THEN
				 				
		ls_errores		=	''
		ls_productor	=	''
		li_secuen		=	Integer(Mid(ls_datos,2,2))			//	Nro.Secuen
		ls_palleti		=	Mid(ls_datos,4,1)
		ls_aparie		=	Mid(ls_datos,5,1)
		li_nropaq		=	Integer(Mid(ls_datos,6,2))			
		li_nrorac		=	Integer(Mid(ls_datos,8,2))
		li_cobay1		=	Integer(Mid(ls_datos,10,3))	
		li_cobay2		=	Integer(Mid(ls_datos,13,3))	
		li_cobay3		=	Integer(Mid(ls_datos,16,3))	
		li_cobay4		=	Integer(Mid(ls_datos,19,3))	
		li_tamba1		=	Integer(Mid(ls_datos,22,3))		
		li_tamba2		=	Integer(Mid(ls_datos,25,3))	
		li_tamba3		=	Integer(Mid(ls_datos,28,3))	
		li_tamba4		=	Integer(Mid(ls_datos,31,3))	
		li_tamba5		=	Integer(Mid(ls_datos,34,3))	
		ls_forade		=	Mid(ls_datos,37,5)
		ls_forap			=  Mid(ls_datos,42,5)
		ls_pesras		=	Mid(ls_datos,47,5)
		ls_pesrab		=	Mid(ls_datos,52,5)
		ls_recidu		=	Mid(ls_datos,57,5)
		ls_mancha		=	Mid(ls_datos,62,5)
		ls_desmod		=	Mid(ls_datos,67,5)
		ls_desalt		=	Mid(ls_datos,72,5)
		ls_deslev		=	Mid(ls_datos,77,5)
		li_punuba		=	Integer(Mid(ls_datos,82,3))	
		li_punini		=	Integer(Mid(ls_datos,85,3))	
		li_punura		=	Integer(Mid(ls_datos,88,3))	
		li_pacida		=	Integer(Mid(ls_datos,91,3))	
		li_opnbay		=	Integer(Mid(ls_datos,94,3))	
		ls_coblan		=	Mid(ls_datos,97,5)
		ls_cocris		=	Mid(ls_datos,102,5)
		ls_codesp		=	Mid(ls_datos,107,5)
		ls_conso2		=	Mid(ls_datos,112,5)
		ls_cobapa		=	Mid(ls_datos,117,5)
		ls_codesg		=	Mid(ls_datos,122,5)	
		ls_pardea		=	Mid(ls_datos,127,5)
		li_dafrio		=	Integer(Mid(ls_datos,132,3))	
		ls_baacuo		=	Mid(ls_datos,135,5)	
		li_coprem		=	Integer(Mid(ls_datos,140,2))
		ls_conden		=	Mid(ls_datos,142,1)
		ls_resemb		=	Mid(ls_datos,143,1)
		ls_rescal		=	Mid(ls_datos,144,1)
		ls_rescon		=	Mid(ls_datos,145,1)
		ls_rescaj		=	Mid(ls_datos,146,4)
		li_caucajl		=	Long(Mid(ls_datos,149,3))	
		

		IF NoExisteDetalle(li_cliente,ls_tipotran, li_nave, li_espe,li_vari, li_etiq, li_prod, &
									  ls_calibr,ls_emba, ld_feemb, li_secuen, ls_errores) THEN

		   ll_nuevo		= dw_1.InsertRow(0)
			dw_1.SetItem(ll_nuevo, "clie_codigo", li_cliente)
			dw_1.SetItem(ll_nuevo, "nave_tipotr", ls_tipotran)
			dw_1.SetItem(ll_nuevo, "nave_codigo", li_nave)
			dw_1.SetItem(ll_nuevo, "espe_codigo", li_espe)
			dw_1.SetItem(ll_nuevo, "vari_codigo", li_vari)
			dw_1.SetItem(ll_nuevo, "etiq_codigo", li_etiq)
			dw_1.SetItem(ll_nuevo, "prod_codigo", li_prod)
			dw_1.SetItem(ll_nuevo, "vaca_calibr", ls_calibr)			
			dw_1.SetItem(ll_nuevo, "emba_codigo", ls_emba)
			dw_1.SetItem(ll_nuevo, "ccde_fecemb", ld_feemb)
			
		   dw_1.SetItem(ll_nuevo, "ccdd_secuen", li_secuen)
		   dw_1.SetItem(ll_nuevo, "ccdd_paleti", ls_palleti)			
		   dw_1.SetItem(ll_nuevo, "ccdd_aparie", ls_aparie)
		   dw_1.SetItem(ll_nuevo, "ccdd_nropaq", li_nropaq)
		   dw_1.SetItem(ll_nuevo, "ccdd_nrorac", li_nrorac)
		   dw_1.SetItem(ll_nuevo, "ccdd_cobay1", li_cobay1)
		   dw_1.SetItem(ll_nuevo, "ccdd_cobay2", li_cobay2)
		   dw_1.SetItem(ll_nuevo, "ccdd_cobay3", li_cobay3)		 
		   dw_1.SetItem(ll_nuevo, "ccdd_cobay4", li_cobay4)			
			dw_1.SetItem(ll_nuevo, "ccdd_tamba1", li_tamba1)
			dw_1.SetItem(ll_nuevo, "ccdd_tamba2", li_tamba2)
			dw_1.SetItem(ll_nuevo, "ccdd_tamba3", li_tamba3)
			dw_1.SetItem(ll_nuevo, "ccdd_tamba4", li_tamba4)
			dw_1.SetItem(ll_nuevo, "ccdd_tamba5", li_tamba5)			
			dw_1.SetItem(ll_nuevo, "ccdd_forade", ls_forade)
			dw_1.SetItem(ll_nuevo, "ccdd_foraap", ls_forap)
			dw_1.SetItem(ll_nuevo, "ccdd_pesras", ls_pesras)
			dw_1.SetItem(ll_nuevo, "ccdd_pesrab", ls_pesrab)
			dw_1.SetItem(ll_nuevo, "ccdd_recidu", ls_recidu)
		   dw_1.SetItem(ll_nuevo, "ccdd_mancha", ls_mancha)
		   dw_1.SetItem(ll_nuevo, "ccdd_desmod", ls_desmod)
		   dw_1.SetItem(ll_nuevo, "ccdd_desalt", ls_desmod)
		   dw_1.SetItem(ll_nuevo, "ccdd_deslev", ls_deslev)
		   dw_1.SetItem(ll_nuevo, "ccdd_punuba", li_punuba)
		   dw_1.SetItem(ll_nuevo, "ccdd_pununi", li_punini)
		   dw_1.SetItem(ll_nuevo, "ccdd_punura", li_punura)
		   dw_1.SetItem(ll_nuevo, "ccdd_pacida", li_pacida)		 
		   dw_1.SetItem(ll_nuevo, "ccdd_opnbay", li_opnbay)			
			dw_1.SetItem(ll_nuevo, "ccdd_coblan", ls_coblan)
			dw_1.SetItem(ll_nuevo, "ccdd_cocris", ls_cocris)
			dw_1.SetItem(ll_nuevo, "ccdd_codesp", ls_codesp)
			dw_1.SetItem(ll_nuevo, "ccdd_conso2", ls_conso2)
			dw_1.SetItem(ll_nuevo, "ccdd_cobapa", ls_cobapa)
		   dw_1.SetItem(ll_nuevo, "ccdd_codesg", ls_codesg)
		   dw_1.SetItem(ll_nuevo, "ccdd_pardea", ls_pardea)
		   dw_1.SetItem(ll_nuevo, "ccdd_dafrio", li_dafrio)
		   dw_1.SetItem(ll_nuevo, "ccdd_baacuo", ls_baacuo)
		   dw_1.SetItem(ll_nuevo, "ccdd_coprem", li_coprem)
		   dw_1.SetItem(ll_nuevo, "ccdd_conden", ls_conden)
		   dw_1.SetItem(ll_nuevo, "ccdd_resemb", ls_resemb)
		   dw_1.SetItem(ll_nuevo, "ccdd_rescal", ls_rescal)		 
		   dw_1.SetItem(ll_nuevo, "ccdd_rescon", ls_rescon)
			dw_1.SetItem(ll_nuevo, "ccdd_rescaj", ls_rescaj)		 
		   dw_1.SetItem(ll_nuevo, "ccdd_caucajl", li_caucajl)
      END IF 
     END IF
LOOP	

dw_1.SetSort("prod_codigo A, pafr_calibr A")
dw_1.Sort()
dw_1.SetRow(1)
dw_1.SelectRow(1,True)
dw_1.SetFocus()
FileClose(li_archivo)

SetPointer(Arrow!)

Message.DoubleParm = li_retorno

end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event ue_seleccion;String	ls_directorio, ls_archivo, ls_prefijo
Integer	li_valida, li_opcion = 1
dwitemstatus stat

ib_ok	= True

IF dw_1.AcceptText() = -1 THEN li_opcion = -1
IF dw_1.ModifiedCount() > 0 THEN 
	li_opcion = 0
END IF

CHOOSE CASE li_opcion
	CASE -1
		ib_ok = False
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.triggerevent("ue_guardar")
				IF message.doubleparm = -1 THEN ib_ok = False
				RETURN 0
			CASE 3
				ib_ok	= False
				RETURN 0
		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN 0

dw_1.Reset()
DO
	li_valida		= GetFileOpenName("Carga de Planilla Destino", ls_directorio, ls_archivo, "con", &
												"Archivos Con*.*")
												
	IF li_valida = 0 THEN
		pb_salir.SetFocus()
		RETURN 0
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE
		ls_archivo			= 	ls_directorio
		ls_prefijo			=  Mid(ls_directorio,25,2)
		gs_arreglo1			=	ls_archivo
		gs_traspasa			=	"F:\GeneradosAconcagua\ProcesadosAconcagua\"+Mid(ls_directorio,23)
		sle_archivo.text	= 	ls_archivo

		Message.DoubleParm = 2
		TriggerEvent("ue_carga_detalle")
		FileClose(li_valida)
	END IF
	
LOOP WHILE Message.DoubleParm = 1

IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled = True
	RETURN 0
ELSE
	pb_grabar.Enabled = False
	RETURN 1
END IF

//
end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0

RETURN 1
end function

public function boolean validadetalle (integer ai_cliente, long al_pallet, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_calibr, long al_cajas, string as_errores);Integer	li_cantid, li_cont
Long		ll_fila, ll_cajas

/*
Valida variedad
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	"dba"."variedades"
	WHERE	espe_codigo	=	:ai_especie
	AND	vari_codigo	=	:ai_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Variedades [" + String(ai_especie) + " " +	String(ai_variedad) + "]"
END IF
/*
Valida Embalaje
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	"dba"."embalajesprod"
	WHERE	clie_codigo	=	:ai_cliente
	AND	emba_codigo	=	:as_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Embalajes [" + as_embalaje + "]"
END IF
/*
Valida Productor
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	"dba"."productores"
	WHERE	prod_codigo	=	:ai_productor;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Productores")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Productores [" + String(ai_productor) + "]"
END IF

IF li_cont>0 THEN
	MessageBox("Error de Consistencia en Existencia","Detalle de Pallets. No Existe Relación de: " + as_errores + ".", &
		StopSign!, Ok!)
	RETURN False
END IF

RETURN True
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_2.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
		END IF
	END IF
ELSE
	IF dw_2.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_1.Update(True, False) = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
			
			dw_2.ResetUpdate()
			dw_1.ResetUpdate()
		END IF
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean noexistedetalle (integer ai_cliente, string as_tipotr, integer ai_nave, integer ai_especie, integer ai_variedad, integer ai_etiqueta, integer ai_productor, string as_calibr, string as_embalaje, date ad_fecha, integer ai_secuen, string as_errores);Integer	li_cantid, li_cont
Long		ll_fila, ll_cajas

ll_fila	=	dw_1.Find ( " clie_codigo = " + String(ai_cliente) + &
								" AND nave_tipotr = '" + as_tipotr + "'"  + &
								" AND nave_codigo = " + String(ai_nave) + &
								" AND espe_codigo = " + String(ai_especie) + &
								" AND vari_codigo = " + String(ai_variedad) + &
								" AND etiq_codigo = " + String(ai_etiqueta) + &
								" AND prod_codigo = " + String(ai_productor)+ &								
								" AND vaca_calibr = '" + as_calibr + "'" + &
								" AND emba_codigo = '" + as_embalaje + "'" + &
								" AND String(ccdd_fecemb) = '" + String(ad_fecha) + "'"+ &
								" AND ccdd_secuen = " + String(ai_secuen) , 1, dw_1.RowCount())				
IF ll_fila > 0 THEN
	RETURN False
END IF

RETURN True
end function

public function boolean validaencabezado (integer ai_especie, integer ai_variedad, integer ai_productor, integer ai_inspector, integer ai_etiqueta, integer ai_puerto, integer ai_recibidor, long ll_nuevo, string as_errores);Integer	li_cantid, li_cont
String	ls_nomesp, ls_varinom, ls_nometiq, ls_nomprod, ls_nominsp,ls_nompuer,&
			ls_nomreci									
/*
Valida Especie
*/
SELECT	espe_nombre
	INTO	:ls_nomesp
	FROM	dba.especies
	WHERE	espe_codigo =:ai_especie;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Especie")
	RETURN False
ELSEIF IsNull(ls_nomesp) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~rEspecie [" + String(ai_especie) +"]"
END IF
/*
Valida Etiqueta
*/
SELECT	etiq_nombre
	INTO	:ls_nometiq
	FROM	dba.etiquetas
	WHERE	etiq_codigo =:ai_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Etiqueta")
	RETURN False
ELSEIF IsNull(ls_nometiq) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~rEtiqueta [" + String(ai_etiqueta) +"]"
END IF
/*
Valida Productor
*/
SELECT 	prod_nombre
	INTO	:ls_nomprod
	FROM	dba.productores
	WHERE	prod_codigo =:ai_productor;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Productores")
	RETURN False
ELSEIF IsNull(ls_nomprod) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Productor [" + String(ai_productor) +"]"
END IF
/*
Valida Variedad
*/
SELECT	vari_nombre
	INTO	:ls_varinom
	FROM	dba.variedades
	WHERE	vari_codigo =:ai_variedad
	AND	espe_codigo =:ai_especie;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedad")
	RETURN False
ELSEIF IsNull(ls_varinom) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Variedad [" + String(ai_variedad) + "]"
END IF
/*
Valida Inspector
*/
SELECT	ccin_nombre
	INTO	:ls_nominsp
	FROM	dba.ctlcalinspectores
	WHERE	ccin_codigo=:ai_Inspector;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Inspector")
	RETURN False
ELSEIF IsNull(ls_nominsp) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Inspector [" + String(ai_Inspector) + "]"
END IF
/*
Valida Puerto
*/
SELECT	puer_nombre
	INTO	:ls_nompuer
	FROM	dba.puertos
	WHERE	puer_codigo=:ai_puerto;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Puerto")
	RETURN False
ELSEIF IsNull(ls_nompuer) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Puerto [" + String(ai_puerto) + "]"
END IF	
/*
Valida Recibidor
*/
SELECT	reci_nombre
	INTO	:ls_nomreci
	FROM	dba.recibidores
	WHERE	reci_codigo=:ai_recibidor;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recibidor")
	RETURN False
ELSEIF IsNull(ls_nomreci) THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Recibidor [" + String(ai_recibidor) + "]"
END IF	
IF li_cont>0 THEN
	MessageBox("Error de Consistencia en Existencia","Encabezado de Planilla. No Existe Relación de: " + as_errores + ".", &
		StopSign!, Ok!)
	RETURN False
ELSE	
	istr_Mant.Argumento[1] = ls_nomesp
	istr_Mant.Argumento[2] = ls_nometiq 
	istr_Mant.Argumento[3] = ls_nomprod 
	istr_Mant.Argumento[4] = ls_varinom 
	istr_Mant.Argumento[5] = ls_nominsp 
	istr_Mant.Argumento[6] = ls_nompuer 
	istr_Mant.Argumento[7] = ls_nomreci 
END IF

RETURN True
end function

event closequery;CHOOSE CASE wf_modifica()
	CASE -1
		Message.ReturnValue = 1 
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.triggerevent("ue_guardar")
				IF message.doubleparm = -1 THEN Message.ReturnValue = 1
				RETURN
			CASE 3
				Message.ReturnValue = 1
				RETURN
		END CHOOSE
END CHOOSE
end event

event open;x	=0
y	=0
im_menu		= m_principal
This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

This.ParentWindow().ToolBarVisible	=	False
dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '"+ This.Title)
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

pb_archivo.PostEvent(Clicked!)


end event

on w_carga_archivo_destino.create
this.dw_2=create dw_2
this.sle_registro=create sle_registro
this.sle_archivo=create sle_archivo
this.st_2=create st_2
this.st_1=create st_1
this.pb_imprimir=create pb_imprimir
this.pb_archivo=create pb_archivo
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.Control[]={this.dw_2,&
this.sle_registro,&
this.sle_archivo,&
this.st_2,&
this.st_1,&
this.pb_imprimir,&
this.pb_archivo,&
this.dw_1,&
this.pb_salir,&
this.pb_grabar}
end on

on w_carga_archivo_destino.destroy
destroy(this.dw_2)
destroy(this.sle_registro)
destroy(this.sle_archivo)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_imprimir)
destroy(this.pb_archivo)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
end on

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= This.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF

IF li_vta = 1 THEN
	This.ParentWindow().ToolBarVisible	= False
	im_menu.Item[1].Item[6].Enabled		= False
	im_menu.Item[7].Visible					= False
	
END IF
end event

type dw_2 from uo_dw within w_carga_archivo_destino
integer x = 78
integer y = 1320
integer width = 3063
integer height = 696
integer taborder = 20
boolean titlebar = true
string title = " Detalle CtlCalDestinoDet"
string dataobject = "dw_cambiar_nombre"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF
end event

event dwnkey;call super::dwnkey;This.SetRedraw(False)
IF KeyDown(KeyRightArrow!) or KeyDown(KeyLeftArrow!) THEN RETURN -1
IF KeyDown(KeyDownArrow!) THEN
	IF (This.GetRow() + 1) <= (This.RowCount()) THEN
		This.SelectRow(0, False)
		This.SelectRow(This.GetRow() + 1, True)
	END IF
ELSE
	IF KeyDown(KeyUpArrow!) THEN
		IF This.GetRow() > 1 THEN
			This.SelectRow(0, False)
			This.SelectRow(This.GetRow() - 1, True)
		END IF
	END IF
END IF

This.SetRedraw(True)
end event

type sle_registro from singlelineedit within w_carga_archivo_destino
integer x = 713
integer y = 224
integer width = 2368
integer height = 92
integer taborder = 10
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_archivo from singlelineedit within w_carga_archivo_destino
integer x = 713
integer y = 116
integer width = 2368
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_carga_archivo_destino
integer x = 151
integer y = 124
integer width = 517
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Archivo de Carga"
boolean focusrectangle = false
end type

type st_1 from statictext within w_carga_archivo_destino
integer x = 78
integer y = 68
integer width = 3063
integer height = 284
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_carga_archivo_destino
event clicked pbm_bnclicked
integer x = 3264
integer y = 1152
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\imprimee.bmp"
string disabledname = "\desarrollo\bmp\imprimed.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_imprimir")
end event

type pb_archivo from picturebutton within w_carga_archivo_destino
integer x = 3264
integer y = 796
integer width = 155
integer height = 132
integer taborder = 10
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\buscaarc.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_seleccion")
end event

type dw_1 from uo_dw within w_carga_archivo_destino
integer x = 78
integer y = 388
integer width = 3063
integer height = 908
integer taborder = 0
boolean titlebar = true
string title = " Detalle CtlCalDestinoDet"
string dataobject = "dw_gene_ctlcaldestinodet"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF
end event

event dwnkey;call super::dwnkey;This.SetRedraw(False)
IF KeyDown(KeyRightArrow!) or KeyDown(KeyLeftArrow!) THEN RETURN -1
IF KeyDown(KeyDownArrow!) THEN
	IF (This.GetRow() + 1) <= (This.RowCount()) THEN
		This.SelectRow(0, False)
		This.SelectRow(This.GetRow() + 1, True)
	END IF
ELSE
	IF KeyDown(KeyUpArrow!) THEN
		IF This.GetRow() > 1 THEN
			This.SelectRow(0, False)
			This.SelectRow(This.GetRow() - 1, True)
		END IF
	END IF
END IF

This.SetRedraw(True)
end event

type pb_salir from picturebutton within w_carga_archivo_destino
integer x = 3264
integer y = 1332
integer width = 155
integer height = 132
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_carga_archivo_destino
integer x = 3264
integer y = 972
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_1 from groupbox within w_carga_archivo_destino
integer x = 3205
integer y = 708
integer width = 274
integer height = 816
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

