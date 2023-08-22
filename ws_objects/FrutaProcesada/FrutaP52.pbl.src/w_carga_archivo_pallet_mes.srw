$PBExportHeader$w_carga_archivo_pallet_mes.srw
$PBExportComments$Recibe archivo plano de producion , de pallets existentes en la zona, de un mes deteminado.
forward
global type w_carga_archivo_pallet_mes from window
end type
type dw_2 from uo_dw within w_carga_archivo_pallet_mes
end type
type sle_archivo from singlelineedit within w_carga_archivo_pallet_mes
end type
type st_2 from statictext within w_carga_archivo_pallet_mes
end type
type st_1 from statictext within w_carga_archivo_pallet_mes
end type
type pb_imprimir from picturebutton within w_carga_archivo_pallet_mes
end type
type pb_archivo from picturebutton within w_carga_archivo_pallet_mes
end type
type pb_salir from picturebutton within w_carga_archivo_pallet_mes
end type
type pb_grabar from picturebutton within w_carga_archivo_pallet_mes
end type
type gb_1 from groupbox within w_carga_archivo_pallet_mes
end type
type dw_1 from uo_dw within w_carga_archivo_pallet_mes
end type
end forward

global type w_carga_archivo_pallet_mes from window
integer width = 3598
integer height = 1832
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
dw_2 dw_2
sle_archivo sle_archivo
st_2 st_2
st_1 st_1
pb_imprimir pb_imprimir
pb_archivo pb_archivo
pb_salir pb_salir
pb_grabar pb_grabar
gb_1 gb_1
dw_1 dw_1
end type
global w_carga_archivo_pallet_mes w_carga_archivo_pallet_mes

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
public function boolean validaencabezado (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_categoria, integer ai_etiqueta, integer ai_condicion, integer ai_planta, string as_errores)
public function boolean validadetalle (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_errores)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexistepacking (integer ai_packing)
public function boolean noexistepallet (integer li_cliente, long ll_nropallet, integer li_planta, ref integer ai_tipopa, ref string as_embalaje, ref integer ai_categoria, ref integer ai_status, ref string as_variedad)
end prototypes

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_carga_detalle();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Integer		li_Retorno, li_Planta, li_Packing, li_CantPallets, ai_tipopa, ai_categoria,&
            ai_status, li_Flag = 0
Long  		ll_Recepcion, ll_GuiaDesp, ll_FilaEncab, ll_FilaDeta, ll_Filas, ll_NroPallet, &
				ll_FilaRecep
String 		ls_Registro, ls_Recepcion,as_embalaje, as_variedad,ls_FechaRecep, ls_Fecha
Date			ld_FechaRecep


dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

ll_Filas	= FileOpen(sle_archivo.text)

IF ll_Filas < 0 THEN
	li_Retorno				= MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!, &
												RetryCancel!)
	Message.DoubleParm	= li_Retorno
	RETURN
ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "Archivo no tiene Filas.")
	Message.DoubleParm	=	2
ELSE
	SetPointer(HourGlass!)
END IF

  DO WHILE ( FileRead(ll_Filas, ls_Registro) >= 0  and li_Flag=0)
	ls_Registro = Left(ls_Registro,44)	  
	IF Len(Trim(ls_Registro)) = 44 THEN
		IF ls_Recepcion <> Mid(ls_Registro,1,12) THEN		
			
			ls_Recepcion	=	Mid(ls_Registro,1,12)
			li_Planta      =  Integer(Mid(ls_Registro, 1, 4))
			ll_Recepcion   =	Long(Mid(ls_Registro, 5, 8))
			ll_FilaRecep	=	dw_2.Find("plde_codigo = " + String(li_Planta) + " AND " + &
									"rfpe_numero = " + String(ll_Recepcion), 1, dw_2.RowCount())
			
			IF ll_FilaRecep   = 0 THEN
				ll_FilaEncab	=  dw_2.InsertRow(0)
				ld_FechaRecep	=	Date(Mid(ls_Registro, 19, 2) + "/" + &
										Mid(ls_Registro, 17, 2) + "/" + &
										Mid(ls_Registro, 13, 4))
				ll_GuiaDesp		=	Long(Mid(ls_Registro, 21, 8))
				li_Packing		=  Integer(Mid(ls_Registro, 29 , 4))
				li_CantPallets =	Integer(Mid(ls_Registro, 33 , 4))
				
				IF NoExistePacking(li_Packing) THEN
					Message.DoubleParm	=	2
					
					RETURN
				END IF
				
				dw_2.SetItem (ll_FilaEncab,"plde_codigo",li_Planta)
				dw_2.SetItem (ll_FilaEncab,"rfpe_numero",ll_Recepcion)
				dw_2.SetItem (ll_FilaEncab,"rfpe_fecrec",ld_FechaRecep)		
				dw_2.SetItem (ll_FilaEncab,"rfpe_Guides",ll_GuiaDesp)
				dw_2.SetItem (ll_FilaEncab,"rfpe_ptaori",li_Packing)
				dw_2.SetItem (ll_FilaEncab,"rfpe_Tarjas",li_CantPallets)
			END IF
		END IF
		
		ll_NroPallet	=	Long(Mid(ls_Registro, 37, 8))
		
		IF NoExistePallet(gi_CodExport, ll_NroPallet,li_Planta, ai_tipopa, &
			as_embalaje,ai_categoria, ai_status, as_variedad) THEN
			Message.DoubleParm	=	2
			
			RETURN
		ELSE
			ll_FilaDeta	=	dw_1.InsertRow(0)
			
			dw_1.SetItem (ll_FilaDeta, "plde_codigo", li_Planta)
			dw_1.SetItem (ll_FilaDeta, "rfpe_numero", ll_Recepcion)
			dw_1.SetItem (ll_FilaDeta, "clie_codigo", gi_CodExport)
			dw_1.SetItem (ll_FilaDeta, "paen_numero", ll_NroPallet)
			dw_1.SetItem (ll_FilaDeta, "paen_tipopa", ai_tipopa)
			dw_1.SetItem (ll_FilaDeta, "emba_codigo", as_embalaje)
			dw_1.SetItem (ll_FilaDeta, "cate_codigo", ai_categoria)
			dw_1.SetItem (ll_FilaDeta, "stat_codigo", ai_status)
			dw_1.SetItem (ll_FilaDeta, "vari_nombre", as_variedad)
		END IF			
	END IF
	LOOP

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
	li_valida		= GetFileOpenName("Carga de Existencias", ls_directorio, ls_archivo, "", &
												"Existencias (Re*.*), Re*.*, Todos los Archivos (*.*), *.*")
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
		
		IF Message.DoubleParm = 2 THEN MessageBox("Atención", "No se cargó archivo exitosamente.")
		
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
	WHERE	emba_codigo	=	:as_embalaje;
	
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
	RETURN False
END IF

RETURN True
end function

public function boolean validadetalle (integer ai_cliente, integer ai_especie, integer ai_variedad, string as_embalaje, integer ai_productor, integer ai_condicion, integer ai_etiqueta, integer ai_planta, string as_errores);Integer	li_cantid, li_cont

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
	as_errores	=	as_errores	+	"~Variedades [" + String(ai_variedad) + "]"
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
//
IF li_cont>0 THEN
	MessageBox("Error de Consistencia","No Existe Relación de: " + as_errores + ".", &
		StopSign!, Ok!)
	RETURN False
END IF

RETURN True
end function

protected function boolean wf_actualiza_db (boolean borrando);if not dw_1.uf_validate(0) then return false

if borrando then
	if dw_1.Update() = 1 AND dw_2.Update() = 1 then
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
	if dw_2.Update() = 1 AND dw_1.Update() = 1  then 
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

public function boolean noexistepacking (integer ai_packing);Integer	li_Cantidad

SELECT	Count(*)
	INTO	:li_Cantidad
	FROM	"dba"."plantadesp"
	WHERE	plde_codigo	=	:ai_Packing ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla plantadesp")
	RETURN True
ELSEIF li_Cantidad = 0 THEN
	IF MessageBox("Atención", "Packing Origen " + String(ai_Packing) + " no existe.~r~r" + &
			"Desea Continuar", Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (integer li_cliente, long ll_nropallet, integer li_planta, ref integer ai_tipopa, ref string as_embalaje, ref integer ai_categoria, ref integer ai_status, ref string as_variedad);SELECT	pa.paen_tipopa, pa.emba_codigo, pa.cate_codigo, pa.stat_codigo, va.vari_nombre
	INTO	:ai_tipopa, :as_embalaje, :ai_categoria, :ai_status, :as_variedad
	FROM	"dba"."palletencab" as pa, "dba"."variedades" as va
	WHERE	pa.clie_codigo	=	:li_Cliente
	AND   pa.plde_codigo =  :li_Planta
	AND   pa.paen_numero =  :ll_NroPallet
	AND   va.clie_codigo =  pa.clie_codigo
	AND   va.espe_codigo =  pa.espe_codigo
   AND   va.vari_codigo =  pa.vari_codigo;
	
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")

	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	IF MessageBox("Atención", "No Existe Pallet " + String(ll_NroPallet) + " para " + &
					"Planta " + String(li_Planta) + "~r~rDesea Continuar el Proceso ?", &
					Question!, YesNo!, 2) = 2 THEN RETURN True
END IF

RETURN False
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

on w_carga_archivo_pallet_mes.create
this.dw_2=create dw_2
this.sle_archivo=create sle_archivo
this.st_2=create st_2
this.st_1=create st_1
this.pb_imprimir=create pb_imprimir
this.pb_archivo=create pb_archivo
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_1=create gb_1
this.dw_1=create dw_1
this.Control[]={this.dw_2,&
this.sle_archivo,&
this.st_2,&
this.st_1,&
this.pb_imprimir,&
this.pb_archivo,&
this.pb_salir,&
this.pb_grabar,&
this.gb_1,&
this.dw_1}
end on

on w_carga_archivo_pallet_mes.destroy
destroy(this.dw_2)
destroy(this.sle_archivo)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_imprimir)
destroy(this.pb_archivo)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_1)
destroy(this.dw_1)
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
dw_1.y					= 321
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41
gb_1.width				= 275
gb_1.height				= 817
gb_1.x 					= This.WorkSpaceWidth() - 351
gb_1.y 					= 493
pb_archivo.x			= This.WorkSpaceWidth() - 292
pb_archivo.y			= 577
pb_archivo.width		= 156
pb_archivo.height		= 133
pb_grabar.x				= pb_archivo.x
pb_grabar.y				= 757
pb_grabar.width		= 156
pb_grabar.height		= 133
pb_imprimir.x			= pb_archivo.x
pb_imprimir.y			= 937
pb_imprimir.width		= 156
pb_imprimir.height	= 133
pb_salir.x				= pb_archivo.x
pb_salir.y				= 1117
pb_salir.width			= 156
pb_salir.height		= 133
end event

type dw_2 from uo_dw within w_carga_archivo_pallet_mes
integer x = 73
integer y = 320
integer width = 3067
integer height = 1236
integer taborder = 10
boolean titlebar = true
string dataobject = "dw_mues_recfruprocee"
end type

type sle_archivo from singlelineedit within w_carga_archivo_pallet_mes
integer x = 709
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

type st_2 from statictext within w_carga_archivo_pallet_mes
integer x = 151
integer y = 124
integer width = 517
integer height = 76
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Archivo de Carga"
boolean focusrectangle = false
end type

type st_1 from statictext within w_carga_archivo_pallet_mes
integer x = 78
integer y = 68
integer width = 3063
integer height = 188
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

type pb_imprimir from picturebutton within w_carga_archivo_pallet_mes
event clicked pbm_bnclicked
integer x = 3232
integer y = 1116
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

type pb_archivo from picturebutton within w_carga_archivo_pallet_mes
integer x = 3227
integer y = 756
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

type pb_salir from picturebutton within w_carga_archivo_pallet_mes
integer x = 3232
integer y = 1296
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
string disabledname = "\Desarrollo 12\Imagenes\Botones\SalirDisab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_carga_archivo_pallet_mes
integer x = 3232
integer y = 936
integer width = 233
integer height = 196
integer taborder = 20
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\gr"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_1 from groupbox within w_carga_archivo_pallet_mes
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

type dw_1 from uo_dw within w_carga_archivo_pallet_mes
integer x = 78
integer y = 320
integer width = 3067
integer height = 1236
integer taborder = 0
boolean titlebar = true
string title = "Datos Cargados"
string dataobject = "dw_mues_recfruproced"
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

