$PBExportHeader$w_maed_planilla_usa.srw
$PBExportComments$Encabezado de Ingreso de Inspección en Destino USA
forward
global type w_maed_planilla_usa from w_mant_encab_deta_csd
end type
end forward

global type w_maed_planilla_usa from w_mant_encab_deta_csd
integer width = 5010
integer height = 2092
string title = "PLANILLA DE INSPECCIÓN DESTINO USA"
string menuname = ""
boolean resizable = false
windowstate windowstate = maximized!
event ue_carga_detalle ( )
end type
global w_maed_planilla_usa w_maed_planilla_usa

type variables
OleObject	myoleobject
DataWindowChild idwc_especies, idwc_mercado
string is_Docname, is_Named
Integer	ii_Hojas

uo_Variedades	   		 iuo_Variedades
uo_Productores	   		 iuo_Productores
uo_Embalajesprod			 iuo_Embalajes
uo_Etiquetas	   		 iuo_Etiquetas
uo_Recibidores	   		 iuo_Recibidores
uo_Puertos		   		 iuo_Puertos
uo_Naves			   		 iuo_Naves
uo_mercado        		 iuo_mercado
uo_ctlcalinspectores     iuo_ctlcalinspectores
uo_sitioinspeccion       iuo_sitioinspeccion
uo_destinos              iuo_destinos

end variables

forward prototypes
protected function integer wf_modifica ()
public function boolean buscaproductor ()
public function boolean buscapuerto ()
public function boolean buscapuertodet (string as_puerto)
public function boolean buscarecibidordet (string as_recibidor)
public function boolean buscarecibidor ()
public function boolean buscanavedet (string as_nave, string as_tipotr)
public function boolean buscanave ()
public function boolean buscaetiquetadet (string as_etiqueta)
public subroutine buscaetiqueta (integer ai_fila)
public function boolean buscavariedad (integer ai_fila)
public subroutine buscaembalaje (integer ai_fila)
public function boolean buscavariedaddet (string as_variedad)
public function string entrega_fecha (string as_fecha)
public function boolean noexistecalibre (integer variedad, string calibre)
public subroutine carga_linea (long al_hoja, integer ai_encab)
public function boolean buscadestino ()
public function boolean buscadestinodet (string as_destino, integer ai_mercado)
public function boolean buscasitio ()
public function boolean buscainspector ()
public function boolean buscasitiodet (string as_sitio)
public function boolean buscainspectordet (string as_inspector)
end prototypes

event ue_carga_detalle();Integer	li_Coneccion, li_Encab, li_Condensacion
Long		ll_Fila, ll_hojas	, ll_nropla
String 	ls_Nave, ls_Puerto, ls_recibidor, ls_Fecha
Date		ld_fecha

SetPointer(HourGlass!)
myoleobject			= CREATE OLEObject 
iuo_Variedades		= CREATE	uo_Variedades
iuo_Productores	= CREATE	uo_Productores
iuo_Embalajes		= CREATE	uo_Embalajesprod

li_Coneccion = myoleobject.ConnectToObject(is_Docname) 
IF li_Coneccion = 0 THEN 
	ll_Hojas = Integer(istr_mant.argumento[13])
	/*Captura nº de hojas de una planilla Excel*/
	ll_nropla= myoleobject.application.workbooks(1).worksheets.count()
	IF ll_hojas <= ll_nropla THEN
		li_Encab = dw_2.InsertRow(0)
		dw_2.Object.num_hojas[li_Encab]	=	Integer(istr_mant.argumento[13])
	   FOR ll_Fila = 5 TO 10		
			 dw_2.Object.clie_codigo[li_Encab]	=	81
			 dw_2.Object.espe_codigo[li_Encab]	=	11
		    IF ll_Fila= 3 	THEN dw_2.Object.cpde_numero[li_Encab]	=	Long(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,54).value)
			
		 
			 dw_2.Object.nave_tipotr[li_Encab]	=	'M'
			 
			 IF ll_Fila = 6 	THEN 
				  //Nave
				 ls_Nave	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,6).value)
			    IF Trim(ls_Nave)  <> Trim(istr_Mant.Argumento[4]) THEN
				    IF BuscaNaveDet(ls_Nave,'M') = False THEN
					    dw_2.Object.nave_nombre[li_Encab]	=	ls_Nave
				    ELSE
					    dw_2.Object.nave_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[4])
					    dw_2.Object.nave_nombre[li_Encab]	=	istr_Mant.Argumento[5]
				    END IF
			    ELSE
				     dw_2.Object.nave_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[4])
				     dw_2.Object.nave_nombre[li_Encab]	=	ls_Nave
			    END IF
				   //Recibidor
					   ls_recibidor = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,49).value)
				   IF Trim(ls_recibidor)  <> Trim(istr_Mant.Argumento[8]) THEN
					   IF BuscaRecibidordet(ls_recibidor) = False THEN
						   dw_2.Object.reci_nombre[li_Encab]	=	ls_recibidor	
					   ELSE
						   dw_2.Object.reci_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[9])
						   dw_2.Object.reci_nombre[li_Encab]	=	istr_Mant.Argumento[8]
					   END IF	
				   ELSE
					   dw_2.Object.reci_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[9])
					   dw_2.Object.reci_nombre[li_Encab]	=	ls_recibidor
				   END IF
					//Fecha Arribo
					ls_Fecha		=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,29).value)
				   IF Len(ls_Fecha) > 10 THEN
					   ls_Fecha = Mid(ls_Fecha,1,10) 
				   END IF
				   dw_2.Object.cpde_fecarr[li_Encab]	=	Date(ls_Fecha)
				 
			  END IF
			
			  IF ll_Fila = 9	THEN 
				//Puerto
				  ls_Puerto = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,6).value)
              IF Trim(ls_Puerto)  <> Trim(istr_Mant.Argumento[7]) THEN
					  IF BuscaPuertoDet(ls_Puerto) = False THEN
						  dw_2.Object.puer_nombre[li_Encab]	=	ls_Puerto
					  ELSE
						  dw_2.Object.puer_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[7])
						  dw_2.Object.puer_nombre[li_Encab]	=	istr_Mant.Argumento[6]
					  END IF
				   ELSE
					  dw_2.Object.puer_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[7])
					  dw_2.Object.puer_nombre[li_Encab]	=	ls_Puerto
				   END IF
					//Fecha Inspección
					 ls_Fecha		=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,29).value)
				    IF not isnull(ls_fecha) or ls_fecha<>"" THEN
				       ld_fecha  = date(ls_fecha)
					    IF  ld_fecha=Date("01-01-1900") THEN
						     Setnull(ld_fecha)
						 ELSE
						     dw_2.Object.cpde_fecins[li_Encab]	=	Date(ls_Fecha)
			          END IF	
			       END IF
			   END IF 
			
			  IF ll_Fila= 7 	THEN 
				//Bodega
					dw_2.Object.cpde_bode01[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,6).value)
//					dw_2.Object.cpde_bode02[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,7).value)
//					dw_2.Object.cpde_bode03[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,8).value)
//					dw_2.Object.cpde_bode04[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,9).value)
//					dw_2.Object.cpde_bode05[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,10).value)
				//Fecha Despacho
				  ls_Fecha		=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,29).value)
				  IF Len(ls_Fecha) > 10 THEN
					  ls_Fecha = Mid(ls_Fecha,1,10) 
				  END IF
				  dw_2.Object.cpde_fecdes[li_Encab]	=	Date(ls_Fecha)
			 END IF
			
			   IF ll_Fila= 8	THEN 
					//Contenedor
					dw_2.Object.cpde_conten[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,6).value)
					//Fecha Fumigación
					ls_Fecha 	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,29).value)
				    IF Len(ls_Fecha) > 10 THEN
					    ls_Fecha = Mid(ls_Fecha,1,2) + Mid(ls_Fecha,6,8)
				    END IF
				    dw_2.Object.cpde_fecfum[li_Encab]	=	Date(ls_Fecha)
					 //Inspector
					 dw_2.Object.ccin_nombre[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,49).value)
				END IF	
				
			   IF ll_Fila= 5 	THEN 
				   dw_2.Object.ccsi_descri[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,29).value)
			      dw_2.Object.dest_nombre[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,6).value)
			   END IF
				
NEXT
		/*Función para Capturar el detalle de la planilla*/
		carga_linea(ll_Hojas,li_Encab)
		myoleobject.disconnectobject()
		Destroy myoleobject
		Destroy iuo_Variedades
		Destroy iuo_Productores
		Destroy iuo_Embalajes	
		Message.DoubleParm = 0
	
   ELSE
	   MessageBox('Atención','Nº de Hojas es Mayor a las existentes en Archivo Excel',Exclamation!)
      dw_2.InSertRow(0)
	   dw_2.Object.clie_codigo[1]	=	81
		dw_2.Object.espe_codigo[1]	=	11
	   dw_2.SetFocus()
	   RETURN
   END IF
   MessageBox('Atención','Proceso Terminado, Debe Grabar',Exclamation!)
   pb_imprimir.Enabled	= True
ELSE
	MessageBox('Atención','Error de Conexión Con Planilla Excel',Exclamation!)
END IF 

end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
RETURN 1
end function

public function boolean buscaproductor ();Boolean lb_Boolean = False

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]	
	lb_Boolean = True	
END IF

RETURN lb_Boolean

end function

public function boolean buscapuerto ();Boolean	lb_Boolean = False

	istr_busq.argum[1]	=	'900'
	
	OpenWithParm(w_busc_puertos, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[2] <> "" THEN
		dw_2.Object.puer_codigo[1] = Integer(istr_busq.argum[3])
		dw_2.Object.puer_nombre[1] = istr_busq.argum[2]
		lb_Boolean = True	
	END IF

RETURN lb_Boolean
end function

public function boolean buscapuertodet (string as_puerto);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Puerto

ls_Puerto	= '%' + As_Puerto + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.Puertos
	WHERE	puer_nombre LIKE (:ls_Puerto)
	AND	puer_codigo <=  900;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Puerto")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
		istr_busq.argum[1]	=	'900'
		istr_busq.argum[2]	=	ls_Puerto
		
		OpenWithParm(w_busc_puertos_archivo_excel, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[3] <> "" THEN
			istr_mant.argumento[7] = istr_busq.argum[3]	
			istr_mant.argumento[6] = istr_busq.argum[4]
			lb_Boolean = True	
		END IF
ELSE
	SELECT	puer_codigo
		INTO	:li_Contador
		FROM	dba.puertos
		WHERE	puer_nombre LIKE (:ls_Puerto)
		AND 	puer_codigo <= 900;
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Puerto")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[7] = String(li_Contador)
		istr_mant.argumento[6] = as_Puerto
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public function boolean buscarecibidordet (string as_recibidor);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Recibidor

ls_Recibidor	= '%' + As_Recibidor + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.recibidores
	WHERE	reci_nombre LIKE (:ls_Recibidor);
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Recibidores")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
		//istr_busq.argum[1]	=	String(as_CodCli)
		istr_busq.argum[2]	=	ls_Recibidor
		OpenWithParm(w_busc_recibidores_plan_dest, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[3] <> "" THEN
			istr_mant.argumento[9] = istr_busq.argum[3]	
			istr_mant.argumento[8] = As_Recibidor
			lb_Boolean = True	
		END IF
ELSE
	SELECT	reci_Codigo
		INTO	:li_Contador
		FROM	dba.recibidores
		WHERE	reci_nombre LIKE (:ls_Recibidor);
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Recibidores")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[9] = String(li_Contador)
		istr_mant.argumento[8] = As_Recibidor
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public function boolean buscarecibidor ();Boolean	lb_Boolean = False

//istr_busq.argum[1]	=	String(as_CodCli)

OpenWithParm(w_busc_recibidores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	
	dw_2.Object.reci_codigo[1] = Integer(istr_busq.argum[1])
	dw_2.Object.reci_nombre[1] = istr_busq.argum[2]
	lb_Boolean = True	
END IF
		

RETURN lb_Boolean
end function

public function boolean buscanavedet (string as_nave, string as_tipotr);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Nave

ls_Nave	= '%' + as_Nave + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.naves
	WHERE	nave_nombre LIKE (:ls_Nave)
	AND	nave_tipotr = :as_Tipotr;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Naves")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
	
		istr_busq.argum[1]	=	as_Tipotr
		istr_busq.argum[2]	=	ls_Nave
		OpenWithParm(w_busc_naves, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[4] <> "" THEN
			istr_mant.argumento[4] = istr_busq.argum[3]
			istr_mant.argumento[5] = istr_busq.argum[4]
			lb_Boolean = True	
		END IF
ELSE
	SELECT	Nave_Codigo
		INTO	:li_Contador
		FROM	dba.naves
		WHERE	nave_nombre LIKE (:ls_Nave)
		AND 	nave_tipotr =:as_Tipotr;
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Naves")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[4] = String(li_Contador)
		istr_mant.argumento[5] = as_Nave
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public function boolean buscanave ();Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Nave

istr_busq.argum[1]	=	dw_2.Object.nave_tipotr[1]
istr_busq.argum[2] = '*'
		
OpenWithParm(w_busc_naves, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	dw_2.Object.nave_codigo[1] = Integer(istr_busq.argum[3])
	dw_2.Object.nave_nombre[1] = istr_busq.argum[4]
	lb_Boolean = True	
END IF

RETURN lb_Boolean
end function

public function boolean buscaetiquetadet (string as_etiqueta);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Etiqueta, ls_Nombre

ls_Etiqueta	= '%' + as_Etiqueta + '%'

SELECT	count(*)
	INTO	:li_contador
	FROM	dba.etiquetas
	WHERE etiq_nombre LIKE (:ls_etiqueta);
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Etiqueta")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
	

		lb_Boolean = True	
ELSE
	SELECT	etiq_codigo, etiq_nombre
		INTO	:li_Contador,:ls_Nombre
		FROM	dba.etiquetas
		WHERE	etiq_nombre LIKE (:ls_Etiqueta);
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Etiqueta")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[11] = String(li_Contador)
		istr_mant.argumento[10] = as_Etiqueta
		istr_mant.argumento[12] = ls_Nombre
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public subroutine buscaetiqueta (integer ai_fila);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Etiqueta, ls_Nombre

	
//istr_busq.argum[3]	=	String(ai_CodCli)
OpenWithParm(w_busc_Etiquetas, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	dw_1.Object.etiq_codigo[ai_Fila] = Integer(istr_busq.argum[1])
	dw_1.Object.etiq_nombre[ai_Fila] = istr_busq.argum[2]
	
END IF


end subroutine

public function boolean buscavariedad (integer ai_fila);Boolean lb_Boolean = False
istr_busq.argum[1]	=	'81'
istr_busq.argum[2]	=	'11'
OpenWithParm(w_busc_variedades, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] <> "" THEN
	dw_1.Object.vari_codigo[ai_Fila] = Integer(istr_busq.argum[4])
	dw_1.Object.vari_nombre[ai_Fila] = istr_busq.argum[5]
	lb_Boolean = True	
END IF

RETURN lb_Boolean
end function

public subroutine buscaembalaje (integer ai_fila);Boolean lb_Boolean = False

istr_busq.argum[1] = '81'

OpenWithParm(w_busc_embalajesprod, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_1.Object.emba_codigo[ai_fila] = istr_busq.argum[2]
	dw_1.Object.emba_nombre[ai_fila] = istr_busq.argum[3]
END IF


end subroutine

public function boolean buscavariedaddet (string as_variedad);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Variedad, ls_Nombre

ls_Variedad	= '%' + as_Variedad + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.variedades
	WHERE	vari_nombre LIKE (:ls_Variedad)
	AND 	espe_codigo = 11;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Variedda")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 0 THEN
		
		istr_busq.argum[1] = String(81)
		istr_busq.argum[2] = String(11)
		
		OpenWithParm(w_busc_variedades, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[4] <> "" THEN
			
			istr_Mant.Argumento[2] = istr_busq.argum[4]
		   istr_mant.argumento[3] = ls_Nombre
			
			lb_Boolean = True	
		END IF
		
ELSE 
	
	SELECT	vari_codigo, vari_nombre
		INTO	:li_Contador,:ls_Nombre
		FROM	dba.variedades
		WHERE	vari_nombre LIKE (:ls_Variedad)
		AND 	espe_codigo = 11;
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Variedad")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_Mant.Argumento[2] = String(li_Contador)
		istr_mant.argumento[3] = ls_Nombre
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean



end function

public function string entrega_fecha (string as_fecha);String Fecha,ls_fecha
ls_fecha = Mid(as_Fecha,1,1)
IF ls_fecha  = '1' THEN
	Fecha='11'
ELSEIF ls_fecha = '2' THEN
	Fecha='12'
ELSEIF ls_fecha = '3' THEN
	Fecha='01'
ELSEIF ls_fecha = '4' THEN
	Fecha='02'
ELSEIF ls_fecha = '5' THEN
	Fecha='03'
ELSEIF ls_fecha = '6' THEN
	Fecha='04'
ELSEIF ls_fecha = '7' THEN
	Fecha='05'
ELSEIF ls_fecha = '8' THEN
	Fecha='06'
ELSEIF ls_fecha = '9' THEN
	Fecha='07'
ELSEIF ls_fecha = '10' THEN
	Fecha='08'
ELSEIF ls_fecha = '11' THEN
	Fecha='09'
ELSEIF ls_fecha = '12' THEN
	Fecha='10'
END IF

return Fecha
end function

public function boolean noexistecalibre (integer variedad, string calibre);String ls_Codigo

Calibre	= Calibre + Fill(" ",3 - Len(Calibre))
ls_codigo	=	''

	SELECT Max(vaca_calibr)
		INTO	:ls_Codigo
		FROM	dba.variecalibre
		WHERE espe_codigo =  :gi_CodEspecie
		AND   :variedad in (0,vari_codigo)
		AND   vaca_calibr	=	:Calibre;
	
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla VarieCalibre")
		RETURN TRUE
	ELSEIF ls_Codigo = '' OR IsNull(ls_codigo) THEN
		MessageBox("Atencion","Código de Calibre no Existe, Ingrese Otro Código",Exclamation!)
		RETURN TRUE
	ELSE	
		RETURN FALSE	
	END IF

		
end function

public subroutine carga_linea (long al_hoja, integer ai_encab);Long       ll_fila, li_Secuen, ll_Hojas, ll_larca
Integer    li_Contador, li_ProdCod, li_detalle, li_encab, li_Ano, ld_deslev, ld_Desmod, ld_DesAlt, li_Condensacion
String     ls_Vari, ls_calib, ls_Emba, ls_Etiq, ls_FecEmb, ls_Conden, ls_guacaj, ls_gualot
Date       ld_FecEmb, ld_fecha
Decimal{1} ld_CalEmb, ld_CalCal,ld_CalCon,ld_Pardea, ld_Desgra,ld_FruBla,ld_DesPed,& 
			ld_PaSeco, ld_PasHum, ld_Traslu, ld_Machuca,ld_Danso2, ld_Aplast

ll_Hojas = al_hoja
li_Encab = ai_encab

ll_Fila = 10
li_Contador = 0
DO WHILE li_Contador < 10
	ll_Fila ++
	li_secuen = Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,1).value)
	IF Not IsNull(li_secuen) THEN
		li_ProdCod  = Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,4).value)
		IF	li_ProdCod > 0 AND NOT IsNull(li_ProdCod) THEN
			li_Detalle = dw_1.InsertRow(0)
			li_Contador = 0
			IF li_ProdCod  <> Integer(istr_Mant.Argumento[1]) THEN
				IF Not iuo_Productores.Existe(Long(li_ProdCod),True,Sqlca) THEN
					DO WHILE Buscaproductor( ) = False 
						MessageBox("Atención","Debe Seleccionar un productor",Exclamation!)
					LOOP	
						li_ProdCod	=	Integer(istr_Mant.Argumento[1])
				ELSE
					istr_Mant.Argumento[1]	=	String(li_ProdCod)
				END IF
			END IF
			dw_1.Object.prod_codigo[li_Detalle]	=	li_ProdCod
				
			dw_1.Object.clie_codigo[li_Detalle]	=	81
			dw_1.Object.cpde_numero[li_Detalle]	=	dw_2.Object.cpde_numero[li_Encab]
			dw_1.Object.cpde_fecins[li_Detalle]	=	dw_2.Object.cpde_fecins[li_Encab]
			dw_1.Object.cpdd_secuen[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,1).value)
			dw_1.Object.cpdd_tamlot[li_Detalle]	=	Long(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,2).value)
			dw_1.Object.cpdd_nropal[li_Detalle]	=	Long(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,3).value)
			dw_1.Object.espe_codigo[li_Detalle]	=	11
			ls_Vari = myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,5).value
			IF TRIM(ls_Vari)  <> Trim(istr_Mant.Argumento[2]) THEN
				IF BuscaVariedadDet(ls_Vari ) = False THEN
					dw_1.Object.vari_nombre[li_Detalle]	=	ls_Vari
				ELSE
					dw_1.Object.vari_codigo[li_Detalle]	=	Integer(istr_Mant.Argumento[2])
					dw_1.Object.vari_nombre[li_Detalle]	=	ls_Vari
				END IF
			ELSE
				dw_1.Object.vari_codigo[li_Detalle]	=	Integer(istr_Mant.Argumento[2])
				dw_1.Object.vari_nombre[li_Detalle]	=	ls_Vari
			END IF
			ls_calib = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,7).value)
			ll_larca = len(ls_calib)
			IF ll_larca=5 THEN
				ls_calib = Mid(ls_calib,1,(ll_larca - 2))
			ELSEIF ll_larca <=2 THEN
				ls_calib = Trim(ls_calib)
			ELSE
			   ls_calib = Mid(ls_calib,1,(ll_larca - 1))
			END IF
			dw_1.Object.cpdd_calibr[li_Detalle]	= ls_calib
			ls_Emba = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,8).value)
			ls_Emba = /*'U' +*/  ls_emba
			IF Trim(ls_Emba)  <> Trim(istr_Mant.Argumento[3]) THEN
				IF Not iuo_Embalajes.Existe(gi_codexport,Trim(ls_Emba),True,Sqlca) THEN
					dw_1.Object.emba_nombre[li_Detalle]	=	ls_Emba
				ELSE
				   dw_1.Object.emba_codigo[li_Detalle]	=	iuo_Embalajes.Codigo
			  		dw_1.Object.emba_nombre[li_Detalle]	=	iuo_Embalajes.Nombre
					istr_Mant.Argumento[3]	=	Trim(ls_Emba)
				END IF				
			ELSE
				dw_1.Object.emba_codigo[li_Detalle]	=	Trim(ls_Emba)
				dw_1.Object.emba_nombre[li_Detalle]	=	iuo_Embalajes.Nombre
			END IF
			ls_Etiq	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,9).value)
			IF Trim(ls_Etiq)  <> Trim(istr_Mant.Argumento[11]) THEN
				IF BuscaEtiquetaDet(ls_Etiq) = False THEN
					dw_1.Object.etiq_nombre[li_Detalle]	=	ls_Etiq
				ELSE
					dw_1.Object.etiq_codigo[li_Detalle]	=	Integer(Istr_Mant.Argumento[11])
					dw_1.Object.etiq_nombre[li_Detalle]	=	ls_Etiq
				END IF
			ELSE
				dw_1.Object.etiq_codigo[li_Detalle]	=	Integer(Istr_Mant.Argumento[11])
				dw_1.Object.etiq_nombre[li_Detalle]	=	ls_Etiq
			END IF
		
			dw_1.Object.cpdd_temper[li_Detalle]	=	dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,10).value)
			
			ls_FecEmb = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,6).value)
				
			IF Integer(Mid(ls_FecEmb,1,1)) > 2 THEN
				li_Ano = Integer(String(Today(),'yyyy'))
			ELSE
				li_Ano = Integer(String(Today(),'yyyy')) - 1
			END IF 
			ld_FecEmb	= Date(Mid(ls_FecEmb,2,2) + '/' +  entrega_fecha(ls_FecEmb) + '/' + String(li_Ano))
			dw_1.Object.cpdd_fecemb[li_Detalle]	=	ld_FecEmb	
			dw_1.Object.cpdd_detpal[li_Detalle]	=	Mid(String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,11).value),1,1)
			dw_1.Object.cpdd_rotula[li_Detalle]	=	Mid(String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,12).value),1,1)
			dw_1.Object.cpdd_matint[li_Detalle]	=	Mid(String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,13).value),1,1)
			dw_1.Object.cpdd_nropaq[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,14).value)
			ld_CalEmb = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,15).value)
			IF ld_CalEmb > 10 THEN 
				ld_CalEmb = ld_CalEmb/10
			END IF
			dw_1.Object.cpdd_calemb[li_Detalle]	=	ld_CalEmb
			dw_1.Object.cpdd_cbsoco[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,16).value)
			dw_1.Object.cpdd_cbmayo[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,17).value)
			dw_1.Object.cpdd_cbbaya[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,18).value)
			dw_1.Object.cpdd_cbmeno[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,19).value)
			dw_1.Object.cpdd_tbayxl[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,20).value)
			dw_1.Object.cpdd_tbayae[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,21).value)
			dw_1.Object.cpdd_tbayar[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,22).value)
			dw_1.Object.cpdd_tba300[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,23).value)
			dw_1.Object.cpdd_tbmeno[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,24).value)
   		dw_1.Object.cpdd_racdef[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,25).value)
			dw_1.Object.cpdd_racapr[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,26).value)
			dw_1.Object.cpdd_rasbpe[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,27).value)
			dw_1.Object.cpdd_rapeba[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,28).value)
			dw_1.Object.cpdd_defman[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,29).value)
			dw_1.Object.cpdd_defgso[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,30).value)
			dw_1.Object.cpdd_defres[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,31).value)
			dw_1.Object.cpdd_grbrix[li_Detalle]	=	dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,32).value)
			
			ld_CalCal = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,33).value)
			IF ld_CalCal > 10 THEN
				ld_CalCal = ld_CalCal/10
			END IF 
			dw_1.Object.cpdd_calcal[li_Detalle]	=	ld_CalCal	
				
			ld_deslev = Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,34).value)
			dw_1.Object.cpdd_deslev[li_Detalle]	=	ld_deslev
				
			ld_Desmod = Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,35).value)
			dw_1.Object.cpdd_desmod[li_Detalle]	=	ld_Desmod
				
			ld_DesAlt = Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,36).value)		
			dw_1.Object.cpdd_desalt[li_Detalle]	=	ld_DesAlt
				
			dw_1.Object.cpdd_pudbay[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,37).value)
			dw_1.Object.cpdd_pudnid[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,38).value)
			dw_1.Object.cpdd_pudrac[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,39).value)
				
			ls_Conden = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,51).value)
			IF Trim(ls_Conden) = 'SI' THEN 
				dw_1.Object.cpdd_conden[li_Detalle]	=	1
			ELSEIF Trim(ls_Conden) = 'NO' THEN
					 dw_1.Object.cpdd_conden[li_Detalle]	=	0
			END IF
			
			dw_1.Object.cpdd_conden[li_Detalle]	=	li_Condensacion
			ld_Pardea = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,41).value)
			dw_1.Object.cpdd_pardea[li_Detalle]	=	ld_Pardea
			ld_Danso2 = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,42).value)
			dw_1.Object.cpdd_danso2[li_Detalle]	=	ld_Danso2
			ld_Machuca = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,43).value)
			dw_1.Object.cpdd_machuc[li_Detalle]	=	ld_Machuca
			ld_Aplast = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,44).value)
			dw_1.Object.cpdd_aplast[li_Detalle]	=	ld_Aplast
		   ld_Traslu = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,49).value)
			dw_1.Object.cpdd_traslu[li_Detalle]	=	ld_Traslu
			dw_1.Object.cpdd_watber[li_Detalle]	=	Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,46).value)
			ld_PasHum = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,47).value)
			dw_1.Object.cpdd_parhum[li_Detalle]	=	ld_PasHum
			ld_PaSeco = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,48).value)
			dw_1.Object.cpdd_paseco[li_Detalle]	=	ld_PaSeco
			ld_DesPed = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,45).value)
			dw_1.Object.cpdd_desped[li_Detalle]	=	ld_DesPed
			ld_FruBla = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,50).value)
			dw_1.Object.cpdd_frubla[li_Detalle]	=	ld_FruBla
			ld_Desgra = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,40).value)
         dw_1.Object.cpdd_desgra[li_Detalle]	=	ld_Desgra
			ld_CalCon = Dec(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,52).value)
			IF ld_CalCon > 10 THEN
				ld_CalCon = ld_CalCon/10
			END IF 
			dw_1.Object.cpdd_calcon[li_Detalle]	=	ld_CalCon
			ls_GuaCaj = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,53).value)
			IF Trim(ls_GuaCaj) = 'SI' THEN 
				dw_1.Object.cpdd_guacaj[li_Detalle]	=	1
			ELSEIF Trim(ls_GuaCaj) = 'NO' THEN
			   	 dw_1.Object.cpdd_guacaj[li_Detalle]	=	0
			ELSE
				    dw_1.Object.cpdd_guacaj[li_Detalle] = Integer(ls_GuaCaj)
			END IF
			ls_GuaLot = String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,54).value)
			IF Trim(ls_GuaLot) = 'SI' THEN 
				dw_1.Object.cpdd_gualot[li_Detalle]	=	1
			ELSEIF Trim(ls_GuaLot) = 'NO' THEN
			   dw_1.Object.cpdd_gualot[li_Detalle]	=	0
			ELSE
			   dw_1.Object.cpdd_gualot[li_Detalle] = Integer(ls_GuaLot)		
			END IF
			dw_1.Object.cpdd_nfotos[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,55).value)
			dw_1.Object.cpdd_observ[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,56).value)
		END IF
	ELSE
		li_ProdCod  = Integer(myoleobject.application.workbooks(1).worksheets(ll_Hojas).cells(ll_Fila,4).value)
		IF	li_ProdCod = 0 OR  IsNull(li_ProdCod) THEN
		   li_Contador ++
	   END IF
	END IF
LOOP
end subroutine

public function boolean buscadestino ();Boolean lb_Boolean = False

istr_mant.argumento[14] = String(dw_2.Object.merc_codigo[1])

IF istr_mant.argumento[14] = "" THEN
	MessageBox("Atención","Debe seleccionar un Mercado Previamente")
	RETURN lb_Boolean 
ELSE

   istr_busq.argum[1] = String(dw_2.Object.merc_codigo[1])
	OpenWithParm(w_busc_destinos, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[2] <> "" THEN
		istr_mant.argumento[15] = istr_busq.argum[2]	
		istr_mant.argumento[16] = istr_busq.argum[3]	
		dw_2.Object.dest_codigo[1] = Integer(istr_mant.argumento[15])
		dw_2.Object.dest_nombre[1] = istr_mant.argumento[16]
		lb_Boolean = True	
	END IF
END IF

RETURN lb_Boolean

end function

public function boolean buscadestinodet (string as_destino, integer ai_mercado);Boolean	lb_Boolean = False
Integer	li_Contador
String ls_destino, ls_nombre

ls_destino	= '%' + as_destino + '%'

 SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.destinos
	WHERE	dest_nombre LIKE (:ls_destino)
	AND 	merc_codigo = :ai_mercado;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Destinos")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 0 THEN
	
		istr_busq.argum[1] = String(dw_2.Object.merc_codigo[1])
		
		OpenWithParm(w_busc_destinos, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[2] <> "" THEN
			
			istr_Mant.Argumento[15] = istr_busq.argum[2]
		   istr_mant.argumento[16] = istr_busq.argum[3]
			dw_2.Object.dest_codigo[1] = Integer(istr_Mant.Argumento[15])
			dw_2.Object.dest_nombre[1] = istr_mant.argumento[16]
			
			lb_Boolean = True	
		END IF
	END IF

RETURN lb_Boolean
end function

public function boolean buscasitio ();Boolean lb_Boolean = False

	OpenWithParm(w_busc_sitioinspeccion, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[1] <> "" THEN
		istr_mant.argumento[17] = istr_busq.argum[1]	
		istr_mant.argumento[18] = istr_busq.argum[2]	
		dw_2.Object.ccsi_codigo[1] = Integer(istr_mant.argumento[17])
		dw_2.Object.ccsi_descri[1] = istr_mant.argumento[18]
		lb_Boolean = True	
	END IF
RETURN lb_Boolean

end function

public function boolean buscainspector ();Boolean lb_Boolean = False

	OpenWithParm(w_busc_inspectores, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[1] <> "" THEN
		istr_mant.argumento[19] = istr_busq.argum[1]	
		istr_mant.argumento[20] = istr_busq.argum[2]	
		dw_2.Object.ccin_codigo[1] = Integer(istr_mant.argumento[19])
		dw_2.Object.ccin_nombre[1] = istr_mant.argumento[20]
		lb_Boolean = True	
	END IF
RETURN lb_Boolean

end function

public function boolean buscasitiodet (string as_sitio);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_sitio

ls_sitio	= '%' + as_sitio + '%'

 SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.ctlcalsitioinspeccion
	WHERE	ccsi_descri LIKE (:ls_sitio);
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Sitio Inspección")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 0 THEN
		
		OpenWithParm(w_busc_sitioinspeccion, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[1] <> "" THEN
			istr_mant.argumento[17] = istr_busq.argum[1]	
			istr_mant.argumento[18] = istr_busq.argum[2]
			dw_2.Object.ccsi_codigo[1] = Integer(istr_mant.argumento[17])
			dw_2.Object.ccsi_descri[1] = istr_mant.argumento[18]
			lb_Boolean = True	
		END IF
	END IF

RETURN lb_Boolean
end function

public function boolean buscainspectordet (string as_inspector);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_inspector

ls_inspector	= '%' + as_inspector + '%'

 SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.ctlcalinspectores
	WHERE	ccin_nombre LIKE (:ls_inspector);
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Inspectores")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 0 THEN
		
		OpenWithParm(w_busc_inspectores, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[1] <> "" THEN
			istr_mant.argumento[19] = istr_busq.argum[1]	
			istr_mant.argumento[20] = istr_busq.argum[2]
			dw_2.Object.ccin_codigo[1] = Integer(istr_mant.argumento[19])
			dw_2.Object.ccin_nombre[1] = istr_mant.argumento[20]
			lb_Boolean = True	
		END IF
	END IF

RETURN lb_Boolean
end function

on w_maed_planilla_usa.create
call super::create
end on

on w_maed_planilla_usa.destroy
call super::destroy
end on

event ue_seleccion;Integer li_Valor, li_Hojas

pb_imprimir.Enabled	=	False

istr_Mant.Argumento[1]	=	'0' // Argumento que almacena Cod. Productor
istr_Mant.Argumento[2]	=	'0' // Argumento que almacena Cod. Variedad
istr_Mant.Argumento[3]	=	''  // Argumento que almacena Cod. Embalaje
istr_Mant.Argumento[4]	=	''  // Argumento que almacena Nombre Nave
istr_Mant.Argumento[5]	=	'0' // Argumento que almacena Cod. Nave
istr_Mant.Argumento[6]	=	''  // Argumento que almacena Nombre Puerto
istr_Mant.Argumento[7]	=	'0' // Argumento que almacena Cod. Puerto
istr_mant.argumento[8]  = 	''	 // Argumento que almacena Nombre Recibidor
istr_mant.argumento[9]  = 	'0' // Argumento que almacena Codigo Recibidor	
istr_mant.argumento[10] = 	''	 // Argumento que almacena Nombre Etiqueta
istr_mant.argumento[11] = 	'0' // Argumento que almacena Codigo Etiqueta
istr_mant.argumento[12] = 	'' // Argumento que almacena nombre Etiqueta
ii_Hojas = 1

IF Not IsNull(istr_mant.argumento[13]) AND Integer(istr_mant.argumento[13]) > 0 THEN
	dw_1.Reset()
	dw_2.Reset()
	
	li_Valor = GetFileOpenName("Select File",is_Docname, is_Named, "XLS", &
		 + "Excel Files (*.xls),*.xls," &
		 + "Excel Files (*.xls),*.xls")
		 
	IF li_Valor < 1 THEN
		MessageBox("Atención","Error al Abrir Archivo",Exclamation!)
		RETURN  
	ELSE
		This.TriggerEvent("ue_carga_detalle")
	END IF
ELSE
	MessageBox("Atención","Debe ingresar Número de Hojas de la Planilla",Exclamation!)
	RETURN
END IF
SetPointer(Arrow!)
end event

event ue_recuperadatos;//Long 		ll_fila_e, ll_fila_d, ll_fila_f, respuesta
//String	ls_Usuario
//Integer	li_Grupo
//ls_Usuario	=	Upper(Gstr_Us.Nombre)
//
//DO
//	dw_2.SetRedraw(False)
//	dw_2.Reset()
//	
//	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.argumento[1]), &
//										  Integer(istr_mant.argumento[3]))
//	
//	IF ll_fila_e = -1 THEN
//		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//										Information!, RetryCancel!)
//	ELSE
//		idwc_agronomos.SetTransObject(sqlca)			
//	   idwc_agronomos.Retrieve(0,dw_2.Object.zona_codigo[1])	
//		DO			
//   		ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
//											    Integer(istr_mant.argumento[3]))
//
//			IF ll_fila_d = -1 THEN				
//				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//												Information!, RetryCancel!)
//			ELSE
//				pb_grabar.Enabled		= True
//				pb_ins_det.Enabled	= True
//			END IF	
//			
//		IF ll_fila_d > 0 THEN	
//			
//			li_Grupo = BuscaGrupo(ls_Usuario)
//			
//			IF (li_Grupo = 6 )	OR (li_Grupo = 1)  THEN 				
//				pb_imprimir.Enabled	= True
//				dw_1.SetRow(1)
//				dw_1.SelectRow(1,False)
//				dw_1.SetFocus()				
//			ELSE
//				dw_2.Enabled 				=	False
//				dw_1.Enabled				=	False				
//				istr_mant.Solo_Consulta =	True 		
//				
//			END IF 					
//		ELSE
//			pb_ins_det.SetFocus()				
//		END IF
//			
//		LOOP WHILE respuesta = 1
//		
//		HabilitaEncab(False)
//
//		IF respuesta = 2 THEN Close(This)
//	END IF
//	dw_2.SetRedraw(True)
//LOOP WHILE respuesta = 1
//
//IF respuesta = 2 THEN Close(This)
//		
end event

event ue_nuevo;//Long		ll_modif1, ll_modif2, ll_modif3
//
//ib_ok	= True
//
//istr_busq.argum[1]		=	""
//istr_busq.argum[2]		=	""
//istr_Mant.Argumento[3]	=	""
//
//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
//
//IF Not istr_mant.Solo_Consulta THEN
//	CHOOSE CASE wf_modifica()			
//		CASE -1
//			ib_ok = False			
//		CASE 0
//			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
//			ll_modif2	=	dw_2.GetNextModified(0, Primary!)	
//
//			IF dw_1.RowCount() > 0 THEN
//				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
//					CASE 1
//						Message.DoubleParm = 0
//						This.TriggerEvent("ue_guardar")
//						IF message.DoubleParm = -1 THEN ib_ok = False
//					CASE 3
//						ib_ok	= False
//						RETURN
//				END CHOOSE
//			END IF
//	END CHOOSE
//END IF
//
//IF Not ib_ok THEN RETURN
//
//dw_1.Reset()
//
//HabilitaEncab(True)
//
//pb_eliminar.Enabled	=	False
//pb_eli_det.Enabled	=	False
//pb_ins_det.Enabled	=	False
//pb_grabar.Enabled		=	False
//pb_imprimir.Enabled	=	False
//dw_2.Enabled			=	True
//
//dw_2.SetRedraw(False)
//dw_2.Reset()
//dw_2.InsertRow(0)
//dw_2.SetRedraw(True)
//
//dw_2.SetColumn("plde_codigo")
//dw_2.SetItem(1, "clie_codigo",gi_codexport)
//dw_2.SetItem(1, "zona_codigo",gi_CodZona)
//dw_2.SetItem(1, "plde_codigo",gi_CodPlanta)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;//String	ls_Calificacion, ls_CalCalidad, ls_CalCondicion, ls_CalEmbalaje, &
//			ls_Mensaje,ls_Colu[]
//Integer	li_Cont
//
//
//IF il_fila > 0 THEN
//	pb_eliminar.Enabled	= True
//	pb_grabar.Enabled		= True
//END IF
//
//IF dw_1.RowCount()	>	0	THEN	
//	
//	IF dw_1.Object.cctd_resolu[il_fila] = 'R' THEN 
//		
//		IF Isnull(dw_1.Object.ccda_secuen[il_fila]) OR &
//			dw_1.Object.ccda_secuen[il_fila] = 0 THEN 
//			MessageBox("Atención","Falta Ingresar Causal de Objeción",Exclamation!)
//						dw_1.SetColumn("ccda_secuen")
//		END IF
//		
//		IF Isnull(dw_1.Object.cctd_embal1[il_fila]) OR &
//			dw_1.Object.cctd_embal1[il_fila] = 0 THEN 	
//	
//			IF Isnull(dw_1.Object.cctd_calid1[il_fila]) &
//				OR dw_1.Object.cctd_calid1[il_fila] = 0 THEN 
//	
//				IF Isnull(dw_1.Object.cctd_condi1[il_fila]) OR &
//					dw_1.Object.cctd_condi1[il_fila] = 0 THEN 
//						MessageBox("Atención","Falta Ingresar Causal de Rechazo",Exclamation!)
//						dw_1.SetColumn("cctd_embal1")
//				END IF
//			END IF
//		END IF
//	END IF
//	
//	IF Isnull(dw_1.Object.cctd_folpla[il_fila]) OR &
//			dw_1.Object.cctd_folpla[il_fila] = 0 THEN 
//				MessageBox("Atención","Falta Ingresar Folio Planilla",Exclamation!)
//				dw_1.SetColumn("cctd_folpla")
//		
//	ELSE
//	
//		il_fila = dw_1.InsertRow(0)
//		dw_1.Setfocus()
//		dw_1.ScrollToRow(il_fila)
//		dw_1.SetRow(il_fila)
//		dw_1.SetColumn("cctd_tipins")
//		
//		IF dw_1.RowCount() > 0 THEN
//			dw_1.Object.cctd_secuen[il_fila] = dw_1.RowCount()
//		ELSE
//			dw_1.Object.cctd_secuen[il_fila] = 1
//		END IF	
//	END IF
//ELSE
//	IF dw_1.RowCount()	=	0	THEN il_fila = dw_1.InsertRow(0)	
//	
//	dw_1.Setfocus()
//	dw_1.ScrollToRow(il_fila)
//	dw_1.SetRow(il_fila)
//	dw_1.SetColumn("cctd_tipins")
//	
//	IF dw_1.RowCount() > 0 THEN
//		dw_1.Object.cctd_secuen[il_fila] = dw_1.RowCount()
//	ELSE
//		dw_1.Object.cctd_secuen[il_fila] = 1
//	END IF	
//		
//END IF
//dw_2.SetItem(1, "zona_codigo",Integer(istr_mant.argumento[2]))
end event

event open;x				= 0
y				= 0

This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
//dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

//pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

istr_Mant.Argumento[1]	=	'0' // Argumento que almacena Cod. Productor
istr_Mant.Argumento[2]	=	'0' // Argumento que almacena Cod. Variedad
istr_Mant.Argumento[3]	=	''  // Argumento que almacena Cod. Embalaje
istr_Mant.Argumento[4]	=	''  // Argumento que almacena Nombre Nave
istr_Mant.Argumento[5]	=	'0' // Argumento que almacena Cod. Nave
istr_Mant.Argumento[6]	=	''  // Argumento que almacena Nombre Puerto
istr_Mant.Argumento[7]	=	'0' // Argumento que almacena Cod. Puerto
istr_mant.argumento[8]  = 	''	 // Argumento que almacena Nombre Recibidor
istr_mant.argumento[9]  = 	'0' // Argumento que almacena Codigo Recibidor	
istr_mant.argumento[10] = 	''	 // Argumento que almacena Nombre Etiqueta
istr_mant.argumento[11] = 	'0' // Argumento que almacena Codigo Etiqueta
istr_mant.argumento[12] = 	''  // Argumento que almacena nombre Etiqueta
istr_mant.argumento[13] = 	''  // Argumento que almacena Numero Hoja
istr_mant.argumento[14] =  ''  // Argumento que almacena mercado
istr_mant.argumento[15] =  ''  // Argumento que almacena Código Destino
istr_mant.argumento[16] =  ''  // Argumento que almacena Nombre Destino
istr_mant.argumento[17] =  ''  // Argumento que almacena Sitio Inspección
istr_mant.argumento[18] =  ''  // Argumento que almacena Descripción Sitio Inspección
istr_mant.argumento[19] =  ''  // Argumento que almacena Inspector
istr_mant.argumento[20] =  ''  // Argumento que almacena Nombre Inspector

dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve()
idwc_especies.SetSort("espe_nombre A")
idwc_especies.Sort()

dw_2.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
idwc_mercado.Retrieve()
idwc_mercado.SetSort("merc_nombre A")
idwc_mercado.Sort()

dw_2.Object.num_hojas.Visible = True
dw_2.Object.text_hoja.Visible = True
dw_2.Object.clie_codigo[1]	=	81
dw_2.Object.espe_codigo[1]	=	11
dw_2.SetColumn("num_hojas")
dw_2.SetFocus()




end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN
	IF	dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN	
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
	ELSE
		MessageBox("Atención","No se borrarán registros ya almacenados")
	END IF
ELSE
	MessageBox("Atención","No se borrarán registros ya almacenados")
END If
end event

event ue_imprimir;SetPointer(Arrow!)
Integer	li_fila, li_planta,li_Agronomo, li_Tipo, li_zona
Date		ld_FechaEmbaini, ld_FechaEmbafin


SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME CARGA PLANILLAS EXCEL EN DESTINO: USA'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_planilladestino_usa1"


vinf.dw_1.SetTransObject(sqlca)
									  
li_fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[14]),&
									  istr_mant.argumento[15],&
									  Integer(istr_mant.argumento[16]))		  
						  

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

event ue_antesguardar;Long		ll_Numero, ll_fila = 1
Integer  li_Planta, li_Cliente, li_Secuen, li_cont, li_Causal, li_cont1,li_mercado,&
			li_destino, li_especie
String	ls_Mensaje, ls_colu[], ls_destino
	
IF Isnull(dw_2.Object.puer_codigo[1]) OR dw_2.Object.puer_codigo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Puerto"
	ls_colu[li_cont]	= "puer_codigo"
END IF	

IF Isnull(dw_2.Object.reci_codigo[1]) OR dw_2.Object.reci_codigo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Recibidor"
	ls_colu[li_cont]	= "reci_codigo"
END IF	

IF Isnull(dw_2.Object.nave_codigo[1]) OR dw_2.Object.nave_codigo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Nave"
	ls_colu[li_cont]	= "nave_codigo"
END IF

IF Isnull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Destino"
	ls_colu[li_cont]	= "dest_codigo"
END IF

IF Isnull(dw_2.Object.dest_nombre[1]) OR dw_2.Object.dest_nombre[1] = '' THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nDestino"
	ls_colu[li_cont]	= "dest_nombre"
ELSE
	ls_destino=dw_2.Object.dest_nombre[1]
	IF ls_destino='USA' OR ls_destino='ESTADOS UNIDOS' OR ls_destino='EEUU' then
		dw_2.setitem(1,"merc_codigo",1)
	ELSE
		dw_2.setitem(1,"merc_codigo",3)
	END IF	
END IF

IF Isnull(dw_2.Object.ccsi_codigo[1]) OR dw_2.Object.ccsi_codigo[1] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nLugar Inspección"
	ls_colu[li_cont]	= "ccsi_codigo"
END IF

IF Isnull(dw_2.Object.ccsi_descri[1]) OR dw_2.Object.ccsi_descri[1] = '' THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nDescripción Lugar inspección"
	ls_colu[li_cont]	= "ccsi_descri"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF


FOR ll_Fila = 1 TO dw_1.RowCount()
	IF Isnull(dw_1.Object.emba_codigo[ll_Fila]) OR dw_1.Object.emba_codigo[ll_Fila] = '' THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
		ls_colu[li_cont]	= "emba_codigo"
	END IF
	
	IF Isnull(dw_1.Object.vari_codigo[ll_Fila]) OR dw_1.Object.vari_codigo[ll_Fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
		ls_colu[li_cont]	= "vari_codigo"
	END IF
	
	IF Isnull(dw_1.Object.cpdd_calibr[ll_Fila]) OR dw_1.Object.cpdd_calibr[ll_Fila] = '' THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCalibre"
		ls_colu[li_cont]	= "cpdd_calibr"
	END IF
	
	IF Isnull(dw_1.Object.etiq_codigo[ll_Fila]) OR dw_1.Object.etiq_codigo[ll_Fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Etiqueta"
		ls_colu[li_cont]	= "etiq_codigo"
	END IF
NEXT

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN 
ELSE
	li_Cliente = dw_2.Object.clie_codigo[1]
	li_mercado = dw_2.Object.merc_codigo[1]
	li_destino = dw_2.Object.dest_codigo[1]
	li_especie = dw_2.Object.espe_codigo[1]
	
	IF dw_2.GetItemStatus(1,0,Primary!) = New! OR &
		dw_2.GetItemStatus(1,0,Primary!) = NewModified! THEN
	
		UPDATE 	dba.ctlcalplandestinosenc
			SET 	cpde_numero = 0
			WHERE	1 = 2 ;

        SELECT  IsNull(max(cpde_numero),0) + 1
			INTO 	:ll_numero
			FROM	dba.ctlcalplandestinosenc
			WHERE	clie_codigo	=	:li_Cliente
			AND   merc_codigo =  :li_mercado
			AND   dest_codigo =  :li_destino
			AND   espe_codigo =  :li_especie;

	
		dw_2.SetItem(1,"cpde_numero",ll_numero)

	END IF
	
	FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.SetItem(ll_Fila,"cpde_numero",ll_numero)
		dw_1.SetItem(ll_Fila,"merc_codigo",li_mercado)
		dw_1.SetItem(ll_Fila,"dest_codigo",li_destino)
		dw_1.SetItem(ll_Fila,"espe_codigo",li_especie)
	NEXT
	
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	dw_1.Reset()
	dw_2.Reset()
	dw_2.insertrow(0)
	
	dw_2.GetChild("espe_codigo", idwc_especies)
   idwc_especies.SetTransObject(sqlca)
   idwc_especies.Retrieve()
   idwc_especies.SetSort("espe_nombre A")
   idwc_especies.Sort()

	dw_2.GetChild("merc_codigo", idwc_mercado)
	idwc_mercado.SetTransObject(sqlca)
	idwc_mercado.Retrieve()
	idwc_mercado.SetSort("merc_nombre A")
	idwc_mercado.Sort()
	
	dw_2.Object.clie_codigo[1]	=	81
	dw_2.Object.espe_codigo[1]	=	11
	dw_2.SetColumn("num_hojas")
   dw_2.SetFocus()
	
	
	istr_Mant.Argumento[1]	=	'0' // Argumento que almacena Cod. Productor
	istr_Mant.Argumento[2]	=	'0' // Argumento que almacena Cod. Variedad
	istr_Mant.Argumento[3]	=	''  // Argumento que almacena Cod. Embalaje
	istr_Mant.Argumento[4]	=	''  // Argumento que almacena Nombre Nave
	istr_Mant.Argumento[5]	=	'0' // Argumento que almacena Cod. Nave
	istr_Mant.Argumento[6]	=	''  // Argumento que almacena Nombre Puerto
	istr_Mant.Argumento[7]	=	'0' // Argumento que almacena Cod. Puerto
	istr_mant.argumento[8]  = 	''	 // Argumento que almacena Nombre Recibidor
	istr_mant.argumento[9]  = 	'0' // Argumento que almacena Codigo Recibidor	
	istr_mant.argumento[10] = 	''	 // Argumento que almacena Nombre Etiqueta
	istr_mant.argumento[11] = 	'0' // Argumento que almacena Codigo Etiqueta
	istr_mant.argumento[12] = 	''  // Argumento que almacena nombre Etiqueta
	//This.TriggerEvent("ue_carga_detalle")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_planilla_usa
integer x = 18
integer y = 672
integer width = 4654
integer height = 1288
integer taborder = 80
string title = "Detalle Inspección Destino"
string dataobject = "dw_mues_planilla_destino_det"
boolean hscrollbar = false
boolean hsplitscroll = true
end type

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True



end event

event dw_1::losefocus;AcceptText()
end event

event dw_1::itemchanged;String	ls_Nula, ls_Columna
SetNull(ls_Nula)

ls_Columna	= dwo.name


CHOOSE CASE ls_Columna
		
	CASE "vari_codigo"		
		iuo_Variedades		=	CREATE	uo_Variedades
		
		IF iuo_Variedades.Existe(11,Integer(Data), True, Sqlca) = False THEN
			dw_1.Object.vari_codigo[Row]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_1.Object.vari_codigo[Row]	= iuo_Variedades.Variedad
			dw_1.Object.vari_nombre[Row]	= iuo_Variedades.NombreVariedad	
		END IF							
		Destroy iuo_Recibidores
		
   CASE "etiq_codigo"
		
		iuo_Etiquetas		=	CREATE	uo_Etiquetas
		
		IF iuo_Etiquetas.Existe(Integer(Data), True, Sqlca) = False THEN
			dw_1.Object.etiq_codigo[Row]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_1.Object.etiq_codigo[Row]	= iuo_Etiquetas.Codigo
			dw_1.Object.etiq_nombre[Row]	= iuo_Etiquetas.Nombre	
		END IF							
		Destroy iuo_Etiquetas
		
		
//
CASE "cpdd_calibr"
Integer li_variedad

li_variedad = dw_1.Object.vari_codigo[Row]	

IF NOT IsNull(li_variedad) THEN
	IF data <> '' THEN
	 IF NoExisteCalibre(li_variedad,data) THEN
		dw_1.Object.cpdd_calibr[Row]= ls_Nula
		RETURN 1
	 END IF	
	END IF 
ELSE
	MessageBox("Atención","Previamente Debe Elegir Una Variedad",Exclamation!)
	dw_1.Object.cpdd_calibr[Row]= ls_Nula
	RETURN 1
END IF	
//
		
	CASE "emba_codigo"	
		
		iuo_Embalajes		=	CREATE	uo_Embalajesprod
		
		IF iuo_Embalajes.Existe(gi_codexport,Data, True, Sqlca) = False THEN
			dw_1.Object.emba_codigo[Row]= ls_Nula
			RETURN 1
		ELSE
			dw_1.Object.emba_codigo[Row]	= iuo_Embalajes.Codigo
			dw_1.Object.emba_nombre[Row]	= iuo_Embalajes.Nombre	
		END IF							
		Destroy iuo_Embalajes
	
END CHOOSE


end event

event dw_1::getfocus;//return 0
end event

event dw_1::doubleclicked;//
end event

event dw_1::clicked;String ls_Columna
IF Row > 0 THEN
	ls_Columna = dwo.Name
	CHOOSE CASE ls_Columna
			
		CASE "buscavariedad"
			buscavariedad(row)
			
		CASE "buscaembalaje"
			BuscaEmbalaje(row)
			
		CASE "buscaetiqueta"	
			BuscaEtiqueta(row)
	
	END CHOOSE
END IF
end event

event dw_1::itemerror;call super::itemerror;Return 1 
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_planilla_usa
integer x = 334
integer y = 36
integer width = 4027
integer height = 620
string dataobject = "dw_mues_ctlcalplanilla_destino"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

dw_2.AcceptText()

CHOOSE CASE ls_Columna	
		
	CASE "num_hojas"
		istr_Mant.Argumento[13] = data
		
	CASE "reci_codigo"		
		iuo_Recibidores		=	CREATE	uo_Recibidores
		
		IF iuo_Recibidores.Existe(Integer(Data), True, Sqlca) = False THEN
			dw_2.Object.reci_codigo[1]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_2.Object.reci_codigo[1]	= iuo_Recibidores.Codigo
			dw_2.Object.reci_nombre[1]	= iuo_Recibidores.Nombre	
		END IF			
		
		Destroy iuo_Recibidores
		
	CASE "puer_codigo"
		iuo_Puertos		=	CREATE	uo_Puertos

		IF iuo_Puertos.Existe(Integer(Data), True, Sqlca) = False THEN
			dw_2.Object.puer_codigo[1]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_2.Object.puer_codigo[1]	= iuo_Puertos.Codigo
			dw_2.Object.puer_nombre[1]	= iuo_Puertos.Nombre	
		END IF
		
		Destroy iuo_Puertos
		
	CASE "nave_codigo"
		
		iuo_Naves	=	CREATE	uo_Naves
      IF IsNull(dw_2.Object.nave_tipotr[1]) OR dw_2.Object.nave_tipotr[1] = "" THEN
			MessageBox("Atención","Debe Ingresar Tipo de Transporte Previamente")
			dw_2.Object.nave_codigo[1]= Integer(ls_Nula)
			RETURN 1
		ELSE
			IF iuo_Naves.Existe(Integer(Data),dw_2.Object.nave_tipotr[1], True, Sqlca) = False THEN
				dw_2.Object.nave_codigo[1]= Integer(ls_Nula)
				RETURN 1
			ELSE
				dw_2.Object.nave_codigo[1]	= iuo_Naves.Codigo
				dw_2.Object.nave_nombre[1]	= iuo_Naves.Nombre	
			END IF		
			Destroy iuo_Naves
	END IF
		
	CASE "merc_codigo"
		
		iuo_mercado = CREATE uo_mercado
		IF iuo_mercado.existe(Integer(Data),True,SqlCa) THEN
			istr_Mant.Argumento[14] = data
		ELSE
			dw_2.SetItem(1,ls_Columna,Integer(ls_Nula))
			RETURN 1
		END IF
		Destroy iuo_mercado
		
	CASE "ccin_codigo"
		iuo_ctlcalinspectores = CREATE uo_ctlcalinspectores
		IF iuo_ctlcalinspectores.Existe(SqlCa,Integer(Data),True) THEN
			dw_2.Object.ccin_codigo[1] = iuo_ctlcalinspectores.codigoinspector
			dw_2.Object.ccin_nombre[1] = iuo_ctlcalinspectores.nombreinspector
			istr_Mant.Argumento[19] = data
			istr_Mant.Argumento[20] = dw_2.Object.ccin_nombre[1]
		ELSE
			dw_2.SetItem(1,ls_Columna,Integer(ls_Nula))
			RETURN 1
		END IF
		Destroy iuo_ctlcalinspectores
		
	CASE "ccsi_codigo"
		iuo_sitioinspeccion = CREATE uo_sitioinspeccion
		IF iuo_sitioinspeccion.Existe(Integer(Data),True,SqlCa) THEN
		   dw_2.Object.ccsi_codigo[1] = iuo_sitioinspeccion.codigo
			dw_2.Object.ccsi_descri[1] = iuo_sitioinspeccion.descripcion
			istr_Mant.Argumento[17] = data
			istr_Mant.Argumento[18] = dw_2.Object.ccsi_descri[1]
		ELSE
			dw_2.SetItem(1,ls_Columna,Integer(ls_Nula))
			RETURN 1
		END IF
		Destroy iuo_sitioinspeccion
		
	CASE "dest_codigo"	
		iuo_destinos = CREATE uo_destinos
		IF IsNull(dw_2.Object.merc_codigo[1]) OR dw_2.Object.merc_codigo[1] = 0 THEN
			MessageBox("Atención","Debe Ingresar Mercado Previamente")
			dw_2.SetItem(1,ls_Columna,Integer(ls_Nula))
			RETURN 1
		ELSE
			IF iuo_destinos.Existe(dw_2.Object.merc_codigo[1],Integer(Data),True,SqlCa) THEN
				dw_2.Object.dest_codigo[1] = iuo_destinos.codigo
				dw_2.Object.dest_nombre[1] = iuo_destinos.nombre
				istr_Mant.Argumento[15] = data
				istr_Mant.Argumento[16] = dw_2.Object.dest_nombre[1]
			ELSE
				dw_2.SetItem(1,ls_Columna,Integer(ls_Nula))
				RETURN 1
			END IF
			Destroy iuo_destinos
		END IF
	
END CHOOSE
end event

event dw_2::clicked;CHOOSE CASE dwo.Name
	CASE "buscapuertos"
		BuscaPuerto()
		
	CASE "buscarecibidor"
		BuscaRecibidor()
		
	CASE "buscanave"	
		IF IsNull(dw_2.Object.nave_tipotr[1]) OR dw_2.Object.nave_tipotr[1] = "" THEN
			MessageBox("Atención","Debe ingresar Tipo de Transporte previamente")
			RETURN 1
		ELSE
			BuscaNave()
		END IF
	
	CASE "buscadestino"
		IF Not IsNull(dw_2.Object.merc_codigo[1]) OR dw_2.Object.merc_codigo[1] <> 0 THEN
			IF Not IsNull(dw_2.Object.dest_nombre[1]) OR dw_2.Object.dest_nombre[1] <> "" THEN
				IF NOT Buscadestinodet(dw_2.Object.dest_nombre[1],dw_2.Object.merc_codigo[1]) THEN
						 Buscadestino()
					//MessageBox("Atención","No existe código para este destino, Debe ingresarlo")
				END IF
			ELSE
		   	Buscadestino()
			END IF
		ELSE
			MessageBox("Atención","Seleccione un Mercado Previamente")
			RETURN 1
		END IF
		
	CASE "buscasitio"
		IF Not IsNull(dw_2.Object.ccsi_descri[1]) OR dw_2.Object.ccsi_descri[1] <> "" THEN
			IF NOT Buscasitiodet(dw_2.Object.ccsi_descri[1]) THEN
					 Buscasitio()
			END IF
		ELSE
		   Buscasitio()
		END IF
		
	CASE "buscainspector"	
		IF Not IsNull(dw_2.Object.ccin_nombre[1]) OR dw_2.Object.ccin_nombre[1] <> "" THEN
			IF NOT Buscainspectordet(dw_2.Object.ccin_nombre[1]) THEN
					 Buscainspector()
			END IF
		ELSE
		   Buscainspector()
		END IF

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_planilla_usa
integer x = 4709
integer y = 416
end type

event pb_nuevo::clicked;call super::clicked;dw_1.Reset()
dw_2.Reset()
dw_2.insertrow(0)

dw_2.GetChild("espe_codigo", idwc_especies)
idwc_especies.SetTransObject(sqlca)
idwc_especies.Retrieve()
idwc_especies.SetSort("espe_nombre A")
idwc_especies.Sort()

dw_2.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
idwc_mercado.Retrieve()
idwc_mercado.SetSort("merc_nombre A")
idwc_mercado.Sort()

dw_2.Object.clie_codigo[1]	=	81
dw_2.Object.espe_codigo[1]	=	11
dw_2.SetColumn("num_hojas")
dw_2.SetFocus()
pb_Imprimir.Enabled = False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_planilla_usa
boolean visible = false
integer x = 4709
integer y = 596
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_planilla_usa
integer x = 4709
integer y = 784
boolean enabled = true
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_planilla_usa
integer x = 4709
integer y = 956
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_planilla_usa
integer x = 4709
integer y = 1136
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_planilla_usa
boolean visible = false
integer x = 4709
integer y = 1428
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_planilla_usa
boolean visible = false
integer x = 4709
integer y = 1600
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_planilla_usa
integer x = 4709
integer y = 236
end type

