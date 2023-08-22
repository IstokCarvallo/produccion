$PBExportHeader$w_carga_archivo_pallet.srw
forward
global type w_carga_archivo_pallet from window
end type
type sle_registro from singlelineedit within w_carga_archivo_pallet
end type
type dw_4 from datawindow within w_carga_archivo_pallet
end type
type dw_3 from datawindow within w_carga_archivo_pallet
end type
type dw_2 from datawindow within w_carga_archivo_pallet
end type
type sle_archivo from singlelineedit within w_carga_archivo_pallet
end type
type st_2 from statictext within w_carga_archivo_pallet
end type
type st_1 from statictext within w_carga_archivo_pallet
end type
type pb_imprimir from picturebutton within w_carga_archivo_pallet
end type
type pb_archivo from picturebutton within w_carga_archivo_pallet
end type
type dw_1 from uo_dw within w_carga_archivo_pallet
end type
type pb_salir from picturebutton within w_carga_archivo_pallet
end type
type pb_grabar from picturebutton within w_carga_archivo_pallet
end type
type gb_1 from groupbox within w_carga_archivo_pallet
end type
end forward

global type w_carga_archivo_pallet from window
integer width = 3598
integer height = 2108
boolean titlebar = true
string title = "Carga Pallet desde Producción"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 30586022
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle ( )
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion ( )
event ue_imprimir ( )
sle_registro sle_registro
dw_4 dw_4
dw_3 dw_3
dw_2 dw_2
sle_archivo sle_archivo
st_2 st_2
st_1 st_1
pb_imprimir pb_imprimir
pb_archivo pb_archivo
dw_1 dw_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_1 gb_1
end type
global w_carga_archivo_pallet w_carga_archivo_pallet

type variables
protected:
Long		il_fila
Boolean	ib_datos_ok, ib_borrar, ib_ok
Menu		im_menu

Str_parms	istr_parms
Str_info		istr_info
end variables

forward prototypes
protected function integer wf_modifica ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine existeencabezado (integer ai_cliente, long al_pallet, integer ai_planta)
public subroutine actualizafechas ()
public subroutine existedetalle (integer ai_cliente, long al_pallet, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_calibre)
public function boolean validaencabezado (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_categoria, integer ai_etiqueta, integer ai_condicion, integer ai_planta, string as_errores)
public function boolean validadetalle (integer ai_cliente, long al_pallet, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_calibr, long al_cajas, string as_errores)
end prototypes

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

ActualizaFechas()

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	DECLARE Actualizacion PROCEDURE FOR dba.FProc_FechaUltimaActualiza;
			
	EXECUTE Actualizacion; 

	SetPointer(Arrow!)

	IF SQLCA.SQLCode < 0 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Fechas.")
	ELSE
		Close Actualizacion;
	
   END IF
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_carga_detalle();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Long			ll_nuevo, ll_pallet, ll_cajas
String		ls_datos, ls_emba, ls_calibr, ls_productor, ls_errores
Date			ld_feemb, ld_ferec
Integer		li_retorno = 2, li_archivo, li_cliente = gi_CodExport, li_cate, li_stat, li_trat, li_frio, &
				li_tipo, li_espe, li_vari, li_prod, li_cond, li_etiq,	li_plantaenc, li_plantadet, &
				li_altura, li_estado, li_revis, li_ccalid

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
	IF Mid(ls_datos,1,1) = 'E' THEN
		ls_errores		= 	''
		ll_pallet		=	Long(Mid(ls_datos,2,8))			//	Nro.Pallet
		li_tipo			=	Integer(Mid(ls_datos,10,1))	// Tipo de Pallet
		li_espe			=	Integer(Mid(ls_datos,11,2))	//	Especie
		li_vari			=	Integer(Mid(ls_datos,13,2))	//	Variedad
		ls_emba			=	Trim(Mid(ls_datos,15,10))		//	Embalaje
		li_cate			=	Integer(Mid(ls_datos,25,1))	//	Categoria
		li_etiq			=	Integer(Mid(ls_datos,26,2))	//	Etiqueta
		li_stat			=	Integer(Mid(ls_datos,28,2))	//	Status
		li_trat			=	Integer(Mid(ls_datos,30,2))	//	Tratamiento
		li_frio			=	Integer(Mid(ls_datos,32,1))	//	Frio
		li_cond			= 	Integer(Mid(ls_datos,33,1))	//	Condición
		li_plantaenc	=	Integer(Mid(ls_datos,34,4))	//	Planta
		ld_feemb			=	Date(Mid(ls_datos,47,2) + "/" + &
								Mid(ls_datos,45,2) + "/" + &
								Mid(ls_datos,41,4))				//	Fecha Embalaje
		li_altura		=	Integer(Mid(ls_datos,49,4))	//	Altura Pallet
		ll_cajas			=	Long(Mid(ls_datos,53,7))		//	Nro. Cajas
		ld_ferec			=	Date(Mid(ls_datos,66,2) + "/" + &
								Mid(ls_datos,64,2) + "/" + &
								Mid(ls_datos,60,4))				//	Fecha Recepción
		li_estado		=	Integer(Mid(ls_datos,68,1))	//	Estado
		li_revis			=	Integer(Mid(ls_datos,69,1))	//	Inspección
		li_ccalid		=	Integer(Mid(ls_datos,70,1))	//	Control de Calidad
		
		sle_registro.Text	=	" Procesando Pallet:  [ " + String(ll_pallet) + " ]"
		
		IF ValidaEncabezado(li_cliente, li_espe, li_vari, ls_emba, li_cate, li_etiq, &
			li_cond, li_plantaenc, ls_errores) THEN
			ll_nuevo		= dw_2.InsertRow(0)
			dw_2.SetItem(ll_nuevo, "clie_codigo", li_cliente)
			dw_2.SetItem(ll_nuevo, "paen_numero", ll_pallet)
			dw_2.SetItem(ll_nuevo, "paen_tipopa", li_tipo)
			dw_2.SetItem(ll_nuevo, "espe_codigo", li_espe)
			dw_2.SetItem(ll_nuevo, "vari_codigo", li_vari)
			dw_2.SetItem(ll_nuevo, "emba_codigo", ls_emba)
			dw_2.SetItem(ll_nuevo, "cate_codigo", li_cate)
			dw_2.SetItem(ll_nuevo, "etiq_codigo", li_etiq)
			dw_2.SetItem(ll_nuevo, "stat_codigo", li_stat)
			dw_2.SetItem(ll_nuevo, "trat_codigo", li_trat)
			dw_2.SetItem(ll_nuevo, "frio_codigo", li_frio)
			dw_2.SetItem(ll_nuevo, "cond_codigo", li_cond)
			dw_2.SetItem(ll_nuevo, "plde_codigo", li_plantaenc)
			dw_2.SetItem(ll_nuevo, "paen_fecemb", ld_feemb)
			dw_2.SetItem(ll_nuevo, "paen_altura", li_altura)
			dw_2.SetItem(ll_nuevo, "paen_ccajas", ll_cajas)
			dw_2.SetItem(ll_nuevo, "paen_Fecini", ld_ferec)
			dw_2.SetItem(ll_nuevo, "paen_estado", li_estado)
			dw_2.SetItem(ll_nuevo, "paen_inspec", li_revis)
			dw_2.SetItem(ll_nuevo, "paen_concal", li_ccalid)
		END IF
	ELSEIF Mid(ls_datos,1,1) = 'D' THEN
		ls_errores		=	''
		ls_productor	=	''
		ll_pallet		=	Long(Mid(ls_datos,2,8))			//	Nro.Pallet
		li_espe			=	Integer(Mid(ls_datos,10,2))	//	Especie
		li_vari			=	Integer(Mid(ls_datos,12,2))	//	Variedad
		ls_emba			=	Trim(Mid(ls_datos,14,10))		//	Embalaje
		li_prod			=	Integer(Mid(ls_datos,24,3))	//	Productor
		li_cond			= 	Integer(Mid(ls_datos,27,1))	//	Condición
		li_etiq			=	Integer(Mid(ls_datos,28,2))	//	Etiqueta
		li_plantadet	=	Integer(Mid(ls_datos,30,4))	//	Planta
		ls_calibr		=	Mid(ls_datos,34,3)				//	Calidad ó Calibre 
		ll_cajas			=	Long(Mid(ls_datos,37,7))		//	Nro. Cajas
		
		IF ValidaDetalle(li_cliente,ll_pallet,li_espe,li_vari,ls_emba,li_prod,li_cond,li_etiq,li_plantadet,ls_calibr,ll_cajas,ls_errores) THEN
			ll_nuevo		= dw_1.InsertRow(0)
			dw_1.SetItem(ll_nuevo, "clie_codigo", li_cliente)
			dw_1.SetItem(ll_nuevo, "paen_numero", ll_pallet)
			dw_1.SetItem(ll_nuevo, "espe_codigo", li_espe)
			dw_1.SetItem(ll_nuevo, "vari_codigo", li_vari)
			dw_1.SetItem(ll_nuevo, "emba_codigo", ls_emba)
			dw_1.SetItem(ll_nuevo, "prod_codigo", li_prod)
			dw_1.SetItem(ll_nuevo, "cond_codigo", li_cond)
			dw_1.SetItem(ll_nuevo, "etiq_codigo", li_etiq)
			dw_1.SetItem(ll_nuevo, "plde_codigo", li_plantadet)
			dw_1.SetItem(ll_nuevo, "pafr_calibr", ls_calibr)
			dw_1.SetItem(ll_nuevo, "pafr_ccajas", ll_cajas)
			Existedetalle(li_cliente,ll_pallet,li_espe,li_vari,ls_emba,li_prod,li_cond,li_etiq,li_plantadet,ls_calibr)
		END IF
		ExisteEncabezado(li_cliente,ll_pallet,li_plantaenc)
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

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event ue_seleccion();String	ls_directorio, ls_archivo
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
				RETURN
			CASE 3
				ib_ok	= False
				RETURN
		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()

DO
	ls_directorio	= "C:\Generados\LosAndes"
	ls_archivo		= "F:\Generados\LosAndes"
	li_valida		= GetFileOpenName("Carga de Existencias", ls_directorio, ls_archivo, "", &
												"Existencias (EX*.*), EX*.*, Todos los Archivos (*.*), *.*")
	
	IF li_valida = 0 THEN
		pb_salir.SetFocus()
		RETURN
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE
		ls_archivo			= ls_directorio
		sle_archivo.text	= ls_archivo

		Message.DoubleParm = 2
		TriggerEvent("ue_carga_detalle")
		FileClose(li_valida)
	END IF
	
LOOP WHILE Message.DoubleParm = 1

IF dw_1.RowCount() > 0 THEN
	pb_grabar.Enabled = True
ELSE
	pb_grabar.Enabled = False
END IF
end event

event ue_imprimir();Long		ll_nuevo
String	ls_datos
Integer  li_archivo

istr_info.titulo	= "REVISION TRASPASO CONTABILIDAD"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_revision"

vinf.dw_1.SetTransObject(sqlca)

F_Membrete(vinf.dw_1)
IF gs_Ambiente <> 'Windows' THEN
	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
li_archivo		= FileOpen(sle_archivo.text)

vinf.dw_1.SetRedraw(False)

DO WHILE FileRead(li_archivo, ls_datos) >= 0
	IF Len(ls_datos) > 40 THEN
		ll_nuevo		= vinf.dw_1.InsertRow(0)
		vinf.dw_1.SetItem(ll_nuevo, "tipo_comprobante", Integer(Mid(ls_datos, 113, 1)))
		vinf.dw_1.SetItem(ll_nuevo, "fecha", Date(Mid(ls_datos, 7, 8)))
		vinf.dw_1.SetItem(ll_nuevo, "nro_comprobante", Long(Mid(ls_datos, 114, 5)))
		vinf.dw_1.SetItem(ll_nuevo, "presentacion", Long(Mid(ls_datos, 15, 10)))
		vinf.dw_1.SetItem(ll_nuevo, "productor", Integer(Mid(ls_datos, 4, 3)))
		vinf.dw_1.SetItem(ll_nuevo, "zona", Integer(Mid(ls_datos, 1, 3)))
		vinf.dw_1.SetItem(ll_nuevo, "cod_gasto", Integer(Mid(ls_datos, 26, 4)))
		vinf.dw_1.SetItem(ll_nuevo, "documento", Long(Mid(ls_datos, 119, 8)))
		vinf.dw_1.SetItem(ll_nuevo, "tipo_cambio", Dec(Mid(ls_datos, 91, 9)))
		vinf.dw_1.SetItem(ll_nuevo, "monto_dolar", Dec(Mid(ls_datos, 100, 13)))
		vinf.dw_1.SetItem(ll_nuevo, "glosa", Mid(ls_datos, 34, 50))
	END IF
LOOP	

vinf.dw_1.Sort()
vinf.dw_1.GroupCalc()
vinf.dw_1.SetRedraw(True)

FileClose(li_archivo)

pb_grabar.Enabled = True
end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0

RETURN 1
end function

protected function boolean wf_actualiza_db (boolean borrando);if not dw_1.uf_validate(0) then return false

if borrando then
	if dw_1.Update() = 1 AND dw_2.Update() = 1 AND dw_3.Update() = 1 then
		commit;
		if sqlca.sqlcode <> 0 then
			F_ErrorBaseDatos(sqlca, This.Title)
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then F_ErrorBaseDatos(sqlca, This.Title)
		return false
	end if
else
	if dw_2.Update() = 1 AND dw_1.Update() = 1 AND dw_3.Update() = 1 then 
		commit;
		if sqlca.sqlcode <> 0 then
			F_ErrorBaseDatos(sqlca, This.Title)
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then F_ErrorBaseDatos(sqlca, This.Title)
		return false
	end if
end if
return true
end function

public subroutine existeencabezado (integer ai_cliente, long al_pallet, integer ai_planta);DELETE FROM "dba"."palletencab"
	WHERE	clie_codigo	=	:ai_cliente
	AND	paen_numero	=	:al_pallet 
	AND	plde_codigo	=	:ai_planta ;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Eliminación pallet " + String(al_pallet) + "tabla PalletEncab")
	RETURN
END IF
	
end subroutine

public subroutine actualizafechas ();Long		ll_fila, ll_FilaFecha, ll_NuevaFila
Integer	li_Cliente, li_Planta

dw_3.SetTransObject(sqlca)
dw_3.Retrieve()

dw_4.SetTransObject(sqlca)
dw_4.Retrieve()

FOR ll_fila = 1 TO dw_4.RowCount()
	li_Cliente		=	dw_4.Object.clie_codigo[ll_fila]
	li_Planta		=	dw_4.Object.plde_codigo[ll_fila]
	ll_FilaFecha	=	dw_3.Find("clie_codigo = " + String(li_Cliente) + " and " + &
							"plde_codigo = " + String(li_Planta), 1, dw_3.RowCount())
	
	IF ll_FilaFecha > 0 THEN
		dw_3.Object.fapl_fechac[ll_FilaFecha]	=	Today()
		dw_3.Object.fapl_horact[ll_FilaFecha]	=	Now()
		dw_3.Object.fapl_descri[ll_FilaFecha]	=	String(Today(),'dd/mm/yyyy') + '  ' + &
																String(Now())
	ELSE
		ll_NuevaFila = dw_3.InsertRow(0)															
		dw_3.Object.clie_codigo[ll_NuevaFila]	=	li_cliente
		dw_3.Object.plde_codigo[ll_NuevaFila]	=	li_planta
		dw_3.Object.fapl_fechac[ll_NuevaFila]	=	Today()
		dw_3.Object.fapl_horact[ll_NuevaFila]	=	Now()
		dw_3.Object.fapl_descri[ll_NuevaFila]	=	String(Today(),'dd/mm/yyyy') + '  ' + &
																String(Now())
	END IF
NEXT



end subroutine

public subroutine existedetalle (integer ai_cliente, long al_pallet, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_calibre);DELETE FROM "dba"."palletfruta"
	WHERE	clie_codigo	=	:ai_cliente
	AND	paen_numero	=	:al_pallet
	AND	espe_codigo	=	:ai_especie
	AND	vari_codigo	=	:ai_variedad
	AND	emba_codigo	=	:as_embalaje
	AND	prod_codigo	=	:ai_productor
	AND	cond_codigo	=	:ai_condicion
	AND	etiq_codigo	=	:ai_etiqueta
	AND	plde_codigo	=	:ai_planta
	AND	pafr_calibr	=	:as_calibre
;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Eliminación Pallet " + String(al_pallet) + "tabla PalletFruta")
	RETURN
END IF
	
end subroutine

public function boolean validaencabezado (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_categoria, integer ai_etiqueta, integer ai_condicion, integer ai_planta, string as_errores);Integer	li_cantid, li_cont

/*
Valida Especie
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.especies
	WHERE	espe_codigo	=	:ai_especie;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Especies")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~rEspecies [" + String(ai_especie) +"]"
END IF
/*
Valida variedad
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.variedades
	WHERE	espe_codigo	=	:ai_especie
	AND	vari_codigo	=	:ai_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Variedades [" + String(ai_variedad) +"]"
END IF
/*
Valida Embalaje
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.embalajesprod
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
Valida Categoria
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.categorias
	WHERE	cate_codigo	=	:ai_categoria;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Categorias")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Categorias [" + String(ai_categoria) +"]"
END IF
/*
Valida Etiqueta
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.etiquetas
	WHERE	etiq_codigo	=	:ai_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Etiquetas")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Etiquetas [" + String(ai_etiqueta) + "]"
END IF
/*
Valida Condición
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.condicion
	WHERE	cond_codigo	=	:ai_condicion;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Condición")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Condición [" + String(ai_condicion) + "]"
END IF
/*
Valida Planta
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:ai_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PlantaDesp")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Planta [" + String(ai_planta) + "]"
END IF

IF li_cont>0 THEN
	MessageBox("Error de Consistencia","No Existe Relación de: " + as_errores + ".", &
		StopSign!, Ok!)
END IF

RETURN True
end function

public function boolean validadetalle (integer ai_cliente, long al_pallet, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_calibr, long al_cajas, string as_errores);Integer	li_cantid, li_cont
Long		ll_fila, ll_cajas

ll_fila	=	dw_1.Find ( "clie_codigo = " + String(ai_cliente) + &
								" AND paen_numero = " + String(al_pallet) + &
								" AND espe_codigo = " + String(ai_especie) + &
								" AND vari_codigo = " + String(ai_variedad) + &
								" AND emba_codigo = '" + as_embalaje + "'" + &
								" AND prod_codigo = " + String(ai_productor) + &
								" AND cond_codigo = " + String(ai_condicion) + &
								" AND etiq_codigo = " + String(ai_etiqueta) + &
								" AND plde_codigo = " + String(ai_planta) + &
								" AND pafr_calibr = '" + as_calibr + "'", 1, dw_1.RowCount() )
				
IF ll_fila > 0 THEN
	ll_cajas	=	dw_1.Object.pafr_ccajas[ll_fila]
	ll_cajas	=	ll_cajas + al_cajas
	dw_1.SetItem ( ll_fila, "pafr_ccajas" , ll_cajas )
	RETURN False
END IF


/*
Valida Productor
*/
SELECT	count(*)
	INTO	:li_cantid
	FROM	dba.productores
	WHERE	prod_codigo	=	:ai_productor;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Productores")
	RETURN False
ELSEIF li_cantid = 0 THEN
	li_cont	++
	as_errores	=	as_errores	+	"~Productores [" + String(ai_productor) + "]"
END IF


IF li_cont>0 THEN
	MessageBox("Error de Consistencia","No Existe Relación de: " + as_errores + ".", &
		StopSign!, Ok!)
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

on w_carga_archivo_pallet.create
this.sle_registro=create sle_registro
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_2=create dw_2
this.sle_archivo=create sle_archivo
this.st_2=create st_2
this.st_1=create st_1
this.pb_imprimir=create pb_imprimir
this.pb_archivo=create pb_archivo
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_1=create gb_1
this.Control[]={this.sle_registro,&
this.dw_4,&
this.dw_3,&
this.dw_2,&
this.sle_archivo,&
this.st_2,&
this.st_1,&
this.pb_imprimir,&
this.pb_archivo,&
this.dw_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_1}
end on

on w_carga_archivo_pallet.destroy
destroy(this.sle_registro)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_2)
destroy(this.sle_archivo)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_imprimir)
destroy(this.pb_archivo)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_1)
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

event resize;Integer	maximo

maximo	= dw_1.width

dw_1.x					= 78
//dw_1.y					= 321
//dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41
gb_1.width				= 275
gb_1.height				= 817
gb_1.x 					= This.WorkSpaceWidth() - 351
gb_1.y 					= 493
pb_archivo.x			= This.WorkSpaceWidth() - 292
pb_archivo.y			= 577
pb_archivo.width		= 233
pb_archivo.height		= 196
pb_grabar.x				= pb_archivo.x
pb_grabar.y				= 772
pb_grabar.width		= 233
pb_grabar.height		= 196
pb_imprimir.x			= pb_archivo.x
pb_imprimir.y			= 952
pb_imprimir.width		= 233
pb_imprimir.height	= 196
pb_salir.x				= pb_archivo.x
pb_salir.y				= 1132
pb_salir.width			= 233
pb_salir.height		= 196
end event

type sle_registro from singlelineedit within w_carga_archivo_pallet
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
long backcolor = 33543637
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_carga_archivo_pallet
boolean visible = false
integer x = 1157
integer y = 1592
integer width = 1728
integer height = 116
integer taborder = 60
string title = "none"
string dataobject = "dw_mues_palletfruta_traspaso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_carga_archivo_pallet
boolean visible = false
integer x = 78
integer y = 1588
integer width = 777
integer height = 124
integer taborder = 50
string title = "none"
string dataobject = "dw_mues_fechasactuptas"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_carga_archivo_pallet
integer x = 78
integer y = 1304
integer width = 3063
integer height = 588
integer taborder = 41
boolean titlebar = true
string title = " Detalle PalletEncab"
string dataobject = "dw_mues_palletencab_planos"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
	CASE primary!
		dwitemstatus stat
		stat = This.getitemstatus(row,0,buffer)
		
		IF stat = new! OR stat = newmodified! THEN
			ls_Tipo = "Agregando"
		ELSE
			ls_Tipo = "Actualizando"
		END IF
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type sle_archivo from singlelineedit within w_carga_archivo_pallet
integer x = 713
integer y = 116
integer width = 2368
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_carga_archivo_pallet
integer x = 151
integer y = 124
integer width = 517
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Archivo de Carga"
boolean focusrectangle = false
end type

type st_1 from statictext within w_carga_archivo_pallet
integer x = 78
integer y = 68
integer width = 3063
integer height = 280
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_carga_archivo_pallet
event clicked pbm_bnclicked
integer x = 3227
integer y = 1128
integer width = 233
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\ImprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\ImprimirDisab.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_imprimir")
end event

type pb_archivo from picturebutton within w_carga_archivo_pallet
integer x = 3223
integer y = 768
integer width = 233
integer height = 196
integer taborder = 10
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscaArch.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\BuscaArchDisab.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_seleccion")
end event

type dw_1 from uo_dw within w_carga_archivo_pallet
integer x = 78
integer y = 388
integer width = 3063
integer height = 908
integer taborder = 0
boolean titlebar = true
string title = " Detalle PalletFruta"
string dataobject = "dw_mues_palletfruta"
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

type pb_salir from picturebutton within w_carga_archivo_pallet
integer x = 3223
integer y = 1312
integer width = 233
integer height = 196
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_carga_archivo_pallet
integer x = 3223
integer y = 948
integer width = 233
integer height = 196
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_1 from groupbox within w_carga_archivo_pallet
boolean visible = false
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
long backcolor = 12632256
end type

