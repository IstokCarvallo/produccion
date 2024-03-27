$PBExportHeader$uo_certificaciones.sru
$PBExportComments$Objecto Usuario que valida la Existencia de Embarques
forward
global type uo_certificaciones from nonvisualobject
end type
end forward

global type uo_certificaciones from nonvisualobject
end type
global uo_certificaciones uo_certificaciones

type variables
Long 		Productor, Packing
Integer	Predio, Especie, Certificadora, Protocolo, Categoria, Estado, ReCertificacion
String		Nro_Inscripcion, GGN, Monto, Observacion
Date 		Fecha_Auditoria, Fecha_Expiracion, Fecha_Inscripcion, Fecha_Est_Audiroria
end variables

forward prototypes
public function boolean of_existe (long al_productor, integer ai_predio, integer ai_especie, date ad_fecha, boolean ab_mensaje, transaction at_transaccion)
private function boolean of_existeprotocolo (long al_productor, integer ai_predio, integer ai_especie, integer ai_protocolo, date ad_fecha, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean of_existe (long al_productor, integer ai_predio, integer ai_especie, date ad_fecha, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
Integer	li_Cantidad

SELECT	Count(*)
	Into :li_Cantidad
	FROM	dbo.cert_certificacion_prod c Inner Join dbo.Cert_Protocolo p
                            On c.prot_codigo = p.prot_codigo
	WHERE	c.prod_codigo	=	:al_Productor
		AND c.prpr_codigo = :ai_Predio
		And c.espe_codigo = :ai_Especie
        	And p.prot_nroggn = 1
	Using	At_Transaccion;

If At_Transaccion.SqlCode = -1 Then
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Certificacion de Productores")	
	lb_Retorno =  False
ElseIf sqlca.SQLCode = 100 Then
	If ab_Mensaje Then
		MessageBox("Atención", "Código de Productor: " + String(al_Productor, '00000') + ', Predio Código:  ' + String(ai_Predio, '000') + &
							", Especie Codigo: " + String(ai_Especie, '00') + ", no ha sido~n ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	End  If	
	lb_Retorno =  False
Else
	If li_Cantidad = 0 Then 
		MessageBox("Atencion", "Productor: " + String(al_Productor, '00000') + ", Predio: " + String(ai_Predio, '000') + &
						", Especie Codigo: " + String(ai_Especie, '00') + ",  no tiene certificacion registrada.~rFavor verifique informacion ingresada.")
		lb_Retorno =  False
	ElseIf li_Cantidad > 1 Then 
		MessageBox("Atencion", "Productor: " + String(al_Productor, '00000') + ", Predio: " + String(ai_Predio, '000') + &
						", Especie Codigo: " + String(ai_Especie, '00') + ",  tiene mas de una certificacion registrada, para asignar GGN.~rFavor verifique informacion ingresada.")
		lb_Retorno =  False
	Else
		SELECT	c.prod_codigo, c.prpr_codigo, c.prot_codigo, c.espe_codigo
			Into :Productor, :Predio, :Protocolo, :Especie
			FROM	dbo.cert_certificacion_prod c Inner Join dbo.Cert_Protocolo p
											 On c.prot_codigo = p.prot_codigo
			WHERE	c.prod_codigo	=	:al_Productor
				  AND c.prpr_codigo = :ai_Predio
				  And c.espe_codigo = :ai_Especie
				  And p.prot_nroggn = 1
			Using	At_Transaccion;
		
		If At_Transaccion.SqlCode = -1 Then
			F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Certificacion de Productores")	
			lb_Retorno =  False
		Else
			lb_Retorno = of_ExisteProtocolo(Productor, Predio, Especie, Protocolo, ad_Fecha, False, SQLCA)
		End If
	End IF
End If

Return lb_Retorno
end function

private function boolean of_existeprotocolo (long al_productor, integer ai_predio, integer ai_especie, integer ai_protocolo, date ad_fecha, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
Integer	li_Dias


SELECT	prod_codigo, prpr_codigo, espe_codigo, cert_codigo, prot_codigo, cace_codigo, 
			prec_codigo, cece_nroins, cece_ggngap, cece_fecaud, cece_fecexp, cece_fecins, 
			cece_feesau, cece_montos, cece_observ, cece_recert, cece_packin
	Into :Productor, :Predio, :Especie, :Certificadora, :Protocolo, :Categoria, 
			:Estado, :Nro_Inscripcion, :GGN, :Fecha_Auditoria, :Fecha_Expiracion, :Fecha_Inscripcion, 
			:Fecha_Est_Audiroria, :Monto, :Observacion, :ReCertificacion, :Packing
	FROM	dbo.cert_certificacion_prod 
	WHERE	prod_codigo	=	:al_Productor
		AND prpr_codigo = :ai_Predio
		And espe_codigo = :ai_Especie
		And prot_codigo = :ai_Protocolo
	Using	At_Transaccion;

If At_Transaccion.SqlCode = -1 Then
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Certificacion de Productores")	
	lb_Retorno =  False
ElseIf sqlca.SQLCode = 100 Then
	If ab_Mensaje Then
		MessageBox("Atención", "Código de Productor: " + String(al_Productor, '00000') + ', Predio Código:  ' + String(ai_Predio, '000') + &
							", Especie Codigo: " + String(ai_Especie, '00') + ", Protocolo Codigo: " + String(ai_Protocolo, '000') + &
							", no ha sido~n ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	End  If	
	lb_Retorno =  False
Else
	li_Dias = DaysAfter(ad_Fecha, Fecha_Expiracion)
	
	If li_Dias <= 0 Then
		MessageBox("Atencion", "Certificacion a Expirado con fecha: " + String(Fecha_Expiracion, "dd/mm/yyyy"), Exclamation!, OK!)
		lb_Retorno =  False
	ElseIf li_Dias < 30 Then
		MessageBox("Atencion", "Quedan " + String(li_Dias, '00')+ " dias, para que llege a su vencimiento la certificacion.", Exclamation!, OK!)
	End If	
End If

Return lb_Retorno
end function

on uo_certificaciones.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_certificaciones.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

