$PBExportHeader$w_proc_ordencargapallet.srw
$PBExportComments$Proceso de Cierre Mensual.
forward
global type w_proc_ordencargapallet from w_para_informes
end type
type dw_1 from datawindow within w_proc_ordencargapallet
end type
end forward

global type w_proc_ordencargapallet from w_para_informes
integer width = 3831
integer height = 1164
string title = "Genera Pallet Por Orden Embarque"
boolean controlmenu = false
event ue_validapassword ( )
event ue_imprimir ( )
dw_1 dw_1
end type
global w_proc_ordencargapallet w_proc_ordencargapallet

type variables
str_mant istr_mant
Str_busqueda	istr_busq

DataWindowChild	idwc_planta

uo_plantadesp			iuo_plantadesp
end variables

forward prototypes
public function boolean noexistenumero (string columna, integer tipo)
public subroutine buscaordencarga ()
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		fila


istr_info.titulo	= "ORDEN DE CARGA PALLET"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_ordencargapallet"

vinf.dw_1.SetTransObject(sqlca)


fila = vinf.dw_1.Retrieve(dw_1.Object.tipo_transp[1],dw_1.Object.ocen_numero[1],&
								  dw_1.Object.clie_codigo[1],dw_1.Object.plde_codigo[1])

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

public function boolean noexistenumero (string columna, integer tipo);Integer	li_null, li_Cliente
String	ls_tipoEmb
Long		li_Numero, li_NumOcen

SetNull (li_null)

li_Cliente	=	dw_1.Object.clie_codigo[1]
ls_tipoEmb	= 	dw_1.Object.tipo_transp[1]
li_Numero	= 	Long(dw_1.Object.ocen_numero[1])

CHOOSE CASE tipo
	CASE 1
		li_Numero = Long(columna)
		
	CASE 2
		ls_TipoEmb = columna
		
END CHOOSE

IF IsNull(ls_tipoEmb) = False OR IsNull(li_Numero) = False THEN
	
	SELECT	ocen_numero
		INTO	:li_NumOcen
		FROM	dba.ordencargaenca
		WHERE	nave_tipotr = :ls_TipoEmb
		AND	ocen_numero = :li_numero
		AND   clie_codigo = :li_Cliente;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ORDENCARGAENCA")	
		RETURN TRUE
      
	ELSEIF sqlca.SQLCode = 0 THEN
		RETURN FALSE
	ELSE 
		
		dw_1.Object.ocen_numero[1]	=	 li_null
		RETURN TRUE
	END IF
END IF

end function

public subroutine buscaordencarga ();istr_busq.argum[1]	=	String(dw_1.Object.tipo_transp[1])	
istr_busq.argum[10]	=	String(dw_1.Object.clie_codigo[1])	

OpenWithParm(w_busc_ordencargaenca,istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_1.SetItem(1,"tipo_transp",istr_busq.argum[1])
	dw_1.SetItem(1,"ocen_numero",Long(istr_busq.argum[2]))
	RETURN
ELSE
	RETURN
END IF

end subroutine

on w_proc_ordencargapallet.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_proc_ordencargapallet.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;call super::open;dw_1.Object.clie_codigo[1] = gi_codexport
dw_1.Object.tipo_transp[1] = 'M'

//Planta
dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF
idwc_planta.SetFilter("plde_tipopl = 1")
idwc_planta.Filter()

dw_1.GetChild("clie_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF
dw_1.Object.plde_codigo[1] = gi_codplanta

iuo_plantadesp			=	Create uo_plantadesp

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)
end event

type st_computador from w_para_informes`st_computador within w_proc_ordencargapallet
end type

type st_usuario from w_para_informes`st_usuario within w_proc_ordencargapallet
end type

type st_temporada from w_para_informes`st_temporada within w_proc_ordencargapallet
end type

type p_logo from w_para_informes`p_logo within w_proc_ordencargapallet
end type

type st_titulo from w_para_informes`st_titulo within w_proc_ordencargapallet
integer width = 2917
string text = "Genera Pallet Por Orden Embarque"
end type

type pb_acepta from w_para_informes`pb_acepta within w_proc_ordencargapallet
integer x = 3424
integer y = 416
string picturename = "\desarrollo\bmp\aceptae.bmp"
string disabledname = "\desarrollo\bmp\aceptad.bmp"
end type

event pb_acepta::clicked;String	ls_Trans
Integer	li_cliente,li_Planta
Long		li_NroOrden

SetPointer(HourGlass!)

IF IsNull(dw_1.Object.tipo_transp[1]) OR dw_1.Object.tipo_transp[1] = '' THEN
	MessageBox("Atención", "Debe Ingresar Tipo de Transporte Previamente ",Exclamation!)
	RETURN
ELSEIF IsNull(dw_1.Object.ocen_numero[1]) OR dw_1.Object.ocen_numero[1] = 0 THEN
 MessageBox("Atención", "Debe Ingresar Número de Orden Previamente ",Exclamation!)
	RETURN
ELSEIF IsNull(dw_1.Object.clie_codigo[1]) OR dw_1.Object.clie_codigo[1] = 0 THEN
 MessageBox("Atención", "Debe Ingresar Cliente Previamente ",Exclamation!)
	RETURN
ELSEIF IsNull(dw_1.Object.plde_codigo[1])  OR dw_1.Object.plde_codigo[1] = 0 THEN
 MessageBox("Atención", "Debe Ingresar Planta Previamente ",Exclamation!)
	RETURN
END IF

ls_Trans		=	dw_1.Object.tipo_transp[1]
li_cliente	=	dw_1.Object.clie_codigo[1]
li_NroOrden	=	dw_1.Object.ocen_numero[1]
li_Planta	=	dw_1.Object.plde_codigo[1]

DECLARE GeneraPallet PROCEDURE FOR dba.Cama_GeneraPalletsOrdenEmbarque 
			@TipoTransporte =:ls_Trans,
			@NumeroOrden  =:li_NroOrden,
			@Cliente = :li_cliente,
			@Planta = :li_Planta;
			
EXECUTE GeneraPallet;

IF sqlca.sqlcode < 0 THEN
	F_ErrorBaseDatos(sqlca,"El proceso Carga Pallet Por Orden Carga " +&
		  " no se ejecutó Exitosamente")
ELSE
	Close GeneraPallet;

	MessageBox("Atención", "El proceso Carga Pallet Por Orden Carga " +&
					" se ejecutó Exitosamente")
					
	Parent.TriggerEvent("ue_imprimir")
END IF

end event

type pb_salir from w_para_informes`pb_salir within w_proc_ordencargapallet
integer x = 3424
integer y = 692
end type

type dw_1 from datawindow within w_proc_ordencargapallet
integer x = 242
integer y = 440
integer width = 2917
integer height = 512
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_proc_ordencarga_pallet"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Null
dw_1.accepttext()
SetNull(li_Null)

CHOOSE CASE dwo.Name
	
	CASE "clie_codigo"
			
			idwc_planta.SetTransObject(sqlca)
			IF idwc_planta.Retrieve(Integer(data)) = 0 THEN
				MessageBox("Atención","Falta Registrar Plantas")
				idwc_planta.InsertRow(0)
			END IF
			idwc_planta.SetFilter("plde_tipopl = 1")
			idwc_planta.Filter()

	CASE "plde_codigo"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "plde_codigo", li_Null )
			This.SetFocus()
			RETURN 1
		END IF	
		
	CASE "ocen_numero"
	   IF NoExisteNumero(data, 1) THEN
			dw_1.SetItem(1, "ocen_numero", Long(li_Null))
			RETURN 1
		END IF
	
	CASE "tipo_transp"
	   IF NoExisteNumero(data, 2) THEN
			dw_1.SetItem(1, "ocen_numero", Long(li_Null))
			RETURN 1
		END IF
				
END CHOOSE


end event

event clicked;CHOOSE CASE dwo.Name
	CASE "buscanroorden"
		buscaOrdenCarga()
END CHOOSE	
end event

event itemerror;RETURN 1
end event

