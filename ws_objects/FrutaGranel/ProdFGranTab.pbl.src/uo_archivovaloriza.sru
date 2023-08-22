$PBExportHeader$uo_archivovaloriza.sru
$PBExportComments$Objetos con sus respectivas funciones de Valorizacion
forward
global type uo_archivovaloriza from nonvisualobject
end type
end forward

global type uo_archivovaloriza from nonvisualobject
end type
global uo_archivovaloriza uo_archivovaloriza

type variables
DataStore		ids_Registro, ids_doctosvalori, ids_valorizacion
end variables

forward prototypes
public function boolean genera_archivo (date ad_fecha, integer ai_planta)
end prototypes

public function boolean genera_archivo (date ad_fecha, integer ai_planta);Long	ll_filas, ll_fila, ll_filadet
String   		ls_registro, ls_archivo

ids_Registro			=	Create DataStore
ids_doctosvalori		=	Create DataStore
ids_valorizacion		=	Create DataStore

ids_Registro.dataobject= "dw_registro_traspaso"
ids_Registro.SetTransObject(sqlca)

ids_doctosvalori.dataobject= "dw_mues_spro_controldoctosvalori_genera"
ids_doctosvalori.SetTransObject(sqlca)

ids_valorizacion.dataobject= "dw_mues_spro_controlvalorizacion_genera"
ids_valorizacion.SetTransObject(sqlca)


// Tabla Spro_Controldoctosvalori
ll_filas		= ids_doctosvalori.Retrieve(ad_fecha, ai_planta)

IF ll_filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura ")
	Return FALSE
	
ELSEIF ll_filas = 0 THEN
	MessageBox("Atención", "No hay información con Operación Indicada.~r~rIngrese otra Operación.", &
					Exclamation!, Ok!)
	Return FALSE
ELSEIF ll_filas > 0 THEN

	FOR ll_fila = 1 TO ll_filas
		ls_registro = '1'

		IF ISNULL(ids_doctosvalori.Object.cdva_fecpro[ll_fila]) THEN
			ls_Registro	+= Fill(' ',10)
		ELSE	
			ls_Registro	+=	String(ids_doctosvalori.Object.cdva_fecpro[ll_fila], 'dd/mm/yyyy')
		END IF		
		
		IF ISNULL(ids_doctosvalori.Object.cdva_tipdoc[ll_fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(ids_doctosvalori.Object.cdva_tipdoc[ll_fila], '0')
		END IF		
		
		IF ISNULL(ids_doctosvalori.Object.plde_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(ids_doctosvalori.Object.plde_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(ids_doctosvalori.Object.prod_codigo[ll_fila]) THEN
			ls_Registro	+= Fill('', 4)
		ELSE	
			ls_Registro	+=	String(ids_doctosvalori.Object.prod_codigo[ll_fila], '0000')
		END IF		
			
		IF ISNULL(ids_doctosvalori.Object.cdva_guisii[ll_fila]) THEN
			ls_Registro	+= Fill(' ',8)
		ELSE	
			ls_Registro	+=	String(ids_doctosvalori.Object.cdva_guisii[ll_fila], '00000000')
		END IF		
		
		ll_filadet	=ids_Registro.insertrow(0)		
		ids_Registro.object.registro[ll_filadet]=ls_Registro	
	NEXT
	
// Tabla Spro_controlvalorizacion	
ll_filas	=	ids_valorizacion.retrieve(ad_fecha, ai_planta)

	FOR ll_fila = 1 TO ll_filas
		ls_registro		= '2'
		
		IF ISNULL(ids_valorizacion.Object.cvap_fecpro[ll_fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_fecpro[ll_fila], 'dd/mm/yyyy')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.cvap_tipdoc[ll_fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_tipdoc[ll_fila], '0')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.plde_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.plde_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.prod_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 4)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.prod_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.espe_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 2)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.espe_codigo[ll_fila], '00')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.vari_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 4)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.vari_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.enva_tipoen[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 1)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.enva_tipoen[ll_fila], '0')
		END IF		

		IF ISNULL(ids_valorizacion.Object.enva_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 3)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.enva_codigo[ll_fila], '000')
		END IF		

		IF ISNULL(ids_valorizacion.Object.cate_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 3)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cate_codigo[ll_fila], '000')
		END IF		
		
 	   IF ISNULL(ids_valorizacion.Object.sepl_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 2)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.sepl_codigo[ll_fila], '00')
		END IF		

		IF ISNULL(ids_valorizacion.Object.cvap_totkil[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 10)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_totkil[ll_fila], '0000000000')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.cvap_totcaj[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 7)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_totcaj[ll_fila], '0000000')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.cvap_tipcam[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 6)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_tipcam[ll_fila], '000000')
		END IF		

		IF ISNULL(ids_valorizacion.Object.cvap_vaneus[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 7)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_vaneus[ll_fila], '0000000')
		END IF
		
		IF ISNULL(ids_valorizacion.Object.cvap_vanepe[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 9)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_vanepe[ll_fila], '000000000')
		END IF
		
		IF ISNULL(ids_valorizacion.Object.cvap_totnet[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 12)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_totnet[ll_fila], '000000000000')
		END IF		
		
		IF ISNULL(ids_valorizacion.Object.cvap_valiva[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 12)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_valiva[ll_fila], '000000000000')
		END IF
		
		IF ISNULL(ids_valorizacion.Object.cvap_valtot[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 12)
		ELSE	
			ls_Registro	+=	String(ids_valorizacion.Object.cvap_valtot[ll_fila], '000000000000')
		END IF

		ll_filadet	=ids_Registro.insertrow(0)		
		ids_Registro.object.registro[ll_filadet]=ls_Registro	
		
	NEXT

END IF

IF ids_Registro.SaveAs("C:\Generados\ddd.txt",Text!,False) = -1 THEN
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
	Return FALSE
ELSE
	Return TRUE
END IF
end function

on uo_archivovaloriza.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_archivovaloriza.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

